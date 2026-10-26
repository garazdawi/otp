/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2020-2026. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_binary.h"
#include "beam_catches.h"
#include "beam_load.h"
#include "erl_version.h"
#include "beam_bp.h"
#include "erl_debugger.h"
#include "erl_record.h"

#include "beam_asm.h"
#include "beam_jit_cache.h"
#include "beam_load_timing.h"

#ifdef ADDRESS_SANITIZER
#    include <sanitizer/lsan_interface.h>
#endif

#define INVALID_LAMBDA_INDEX -1

static void init_label(Label *lp);
static int beam_load_finish_cache(LoaderState *stp);

static Uint32 cache_read_u32(const byte *p) {
    return ((Uint32)p[0]) | ((Uint32)p[1] << 8) |
           ((Uint32)p[2] << 16) | ((Uint32)p[3] << 24);
}

static Uint64 cache_read_u64(const byte *p) {
    Uint64 value = 0;
    int shift;

    for (shift = 0; shift < 64; shift += 8) {
        value |= (Uint64)*p++ << shift;
    }
    return value;
}

int beam_load_prepare_emit(LoaderState *stp) {
    stp->ba = beamasm_new_assembler(stp->module,
                                    stp->beam.code.label_count,
                                    stp->beam.code.function_count,
                                    &stp->beam,
                                    stp->jit_cache_requested);

    return beam_load_prepare_metadata(stp);
}

int beam_load_prepare_metadata(LoaderState *stp) {
    BeamCodeHeader *hdr;
    int i;

    /* Initialize code header */
    stp->codev_size = stp->beam.code.function_count + 1;
    hdr = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                     (offsetof(BeamCodeHeader, functions) +
                      sizeof(BeamInstr) * stp->codev_size));

    hdr->num_functions = stp->beam.code.function_count;

    stp->ci = hdr->num_functions + 1;

    hdr->attr_ptr = NULL;
    hdr->attr_size = 0;
    hdr->attr_size_on_heap = 0;
    hdr->compile_ptr = NULL;
    hdr->compile_size = 0;
    hdr->compile_size_on_heap = 0;
    hdr->literal_area = NULL;
    hdr->line_table = NULL;
#ifdef ERTS_ENABLE_BEAM_CODE_DEBUGGER
    hdr->debug = NULL;
#endif
    hdr->md5_ptr = NULL;
    hdr->are_nifs = NULL;
    hdr->debugger_flags = erts_debugger_flags;

    stp->coverage = hdr->coverage = NULL;
    stp->line_coverage_valid = hdr->line_coverage_valid = NULL;
    stp->loc_index_to_cover_id = hdr->loc_index_to_cover_id = NULL;

    hdr->line_coverage_len = 0;

    if (stp->beam.lines.flags & BEAMFILE_FORCE_LINE_COUNTERS) {
        hdr->coverage_mode = ERTS_COV_LINE_COUNTERS;
    } else if ((stp->beam.lines.flags & BEAMFILE_EXECUTABLE_LINE) == 0 &&
               (erts_coverage_mode == ERTS_COV_LINE ||
                erts_coverage_mode == ERTS_COV_LINE_COUNTERS)) {
        /* A line coverage mode is enabled, but there are no
         * executable_line instructions in this module; therefore,
         * turn off coverage for this module. */
        hdr->coverage_mode = ERTS_COV_NONE;
    } else {
        /* Use the system default coverage mode for this module. */
        hdr->coverage_mode = erts_coverage_mode;
    }

    switch (hdr->coverage_mode) {
    case ERTS_COV_FUNCTION:
    case ERTS_COV_FUNCTION_COUNTERS: {
        size_t alloc_size = hdr->num_functions;
        if (hdr->coverage_mode == ERTS_COV_FUNCTION_COUNTERS) {
            alloc_size *= sizeof(Uint);
        }
        stp->coverage = erts_alloc(ERTS_ALC_T_CODE_COVERAGE, alloc_size);
        sys_memset(stp->coverage, 0, alloc_size);
        break;
    }
    case ERTS_COV_LINE:
    case ERTS_COV_LINE_COUNTERS: {
        size_t alloc_size = stp->beam.lines.instruction_count;
        Uint coverage_size;

        if (hdr->coverage_mode == ERTS_COV_LINE) {
            coverage_size = sizeof(byte);
        } else {
            coverage_size = sizeof(Uint);
        }
        stp->coverage = erts_alloc(ERTS_ALC_T_CODE_COVERAGE,
                                   alloc_size * coverage_size);
        sys_memset(stp->coverage, 0, alloc_size * coverage_size);
        stp->line_coverage_valid =
                erts_alloc(ERTS_ALC_T_CODE_COVERAGE, alloc_size);
        sys_memset(stp->line_coverage_valid, 0, alloc_size);
        if (hdr->coverage_mode == ERTS_COV_LINE_COUNTERS) {
            stp->loc_index_to_cover_id =
                    erts_alloc(ERTS_ALC_T_CODE_COVERAGE,
                               alloc_size * sizeof(unsigned));
#ifdef DEBUG
            sys_memset(stp->loc_index_to_cover_id,
                       0xff,
                       alloc_size * sizeof(unsigned));
#endif
        }
        hdr->line_coverage_len = alloc_size;
        break;
    }
    }

    stp->load_hdr = hdr;

    stp->labels = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                             stp->beam.code.label_count * sizeof(Label));
    for (i = 0; i < stp->beam.code.label_count; i++) {
        init_label(&stp->labels[i]);
    }

    stp->lambda_literals = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                      stp->beam.lambdas.count * sizeof(SWord));

    for (i = 0; i < stp->beam.lambdas.count; i++) {
        BeamFile_LambdaEntry *lambda = &stp->beam.lambdas.entries[i];

        if (stp->labels[lambda->label].lambda_index == INVALID_LAMBDA_INDEX) {
            stp->labels[lambda->label].lambda_index = i;
            stp->lambda_literals[i] = ERTS_SWORD_MAX;
        } else {
            beam_load_report_error(__LINE__,
                                   stp,
                                   "lambda already defined for label %i. To "
                                   "fix this, please recompile this module "
                                   "with an OTP " ERLANG_OTP_RELEASE
                                   " compiler.",
                                   lambda->label);
            return 0;
        }
    }

    stp->bif_imports =
            erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                       stp->beam.imports.count * sizeof(BifEntry **));

    for (i = 0; i < stp->beam.imports.count; i++) {
        BeamFile_ImportEntry *import;
        const Export *export;
        int bif_number;

        import = &stp->beam.imports.entries[i];
        export = erts_active_export_entry(import->module,
                                          import->function,
                                          import->arity);

        stp->bif_imports[i] = NULL;

        if (export) {
            bif_number = export->bif_number;

            if (bif_number >= 0) {
                if (bif_number == BIF_load_nif_2) {
                    stp->may_load_nif = 1;
                }

                stp->bif_imports[i] = &bif_table[bif_number];
            }
        }
    }

    stp->current_li = 0;
    if (stp->beam.lines.item_count > 0) {
        stp->line_instr = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                     stp->beam.lines.instruction_count *
                                             sizeof(LineInstr));
        stp->func_line = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                    stp->beam.code.function_count *
                                            sizeof(unsigned int));
    }

    return 1;
}

typedef struct BeamJitCachedFixup_ {
    Uint32 kind;
    Uint32 index;
    Uint64 offset;
    Sint64 addend;
    Uint64 target;
} BeamJitCachedFixup;

enum {
    CACHE_FIXUP_CATCH = 1,
    CACHE_FIXUP_IMPORT,
    CACHE_FIXUP_LAMBDA,
    CACHE_FIXUP_LITERAL,
    CACHE_FIXUP_STRING,
    CACHE_FIXUP_BEAM_ATOM,
    CACHE_FIXUP_GLOBAL_FRAGMENT,
    CACHE_FIXUP_RUNTIME_FUNCTION,
    CACHE_FIXUP_BEAM_ATOM_MOV,
    CACHE_FIXUP_GLOBAL_FRAGMENT_MOV,
    CACHE_FIXUP_RUNTIME_FUNCTION_MOV,
    CACHE_FIXUP_NAMED_OFFSET,
    CACHE_FIXUP_LAMBDA_TRAMPOLINE
};

static int cache_patch_word(LoaderState *stp,
                            Uint64 offset,
                            Sint64 addend,
                            UWord value) {
    UWord patched;

    if (offset > stp->loaded_size ||
        sizeof(patched) > stp->loaded_size - offset) {
        return 0;
    }
    patched = value + addend;
    sys_memcpy((byte *)stp->writable_region + offset,
               &patched,
               sizeof(patched));
    return 1;
}

static int cache_patch_aarch64_mov(LoaderState *stp,
                                   Uint64 offset,
                                   UWord value) {
    Uint32 code[4], rd;
    Uint i;

    if (offset > stp->loaded_size || 16 > stp->loaded_size - offset) {
        return 0;
    }
    sys_memcpy(code, (byte *)stp->writable_region + offset, sizeof(code));
    rd = code[0] & 0x1f;
    if ((code[0] & 0xffe00000) != 0xd2800000) {
        return 0;
    }
    for (i = 1; i < 4; i++) {
        if ((code[i] & 0xffe0001f) !=
            (0xf2800000 | (i << 21) | rd)) {
            return 0;
        }
    }
    code[0] = 0xd2800000 | ((value & 0xffff) << 5) | rd;
    for (i = 1; i < 4; i++) {
        code[i] = 0xf2800000 | (i << 21) |
                  (((value >> (16 * i)) & 0xffff) << 5) | rd;
    }
    sys_memcpy((byte *)stp->writable_region + offset, code, sizeof(code));
    return 1;
}

static int cache_apply_immediate_fixup(LoaderState *stp,
                                       const BeamJitCachedFixup *fixup) {
    UWord value;
    int mov = 0;

    switch (fixup->kind) {
    case CACHE_FIXUP_BEAM_ATOM:
    case CACHE_FIXUP_BEAM_ATOM_MOV:
        if (fixup->index >= (Uint32)stp->beam.atoms.count) {
            return 0;
        }
        value = stp->beam.atoms.entries[fixup->index];
        mov = fixup->kind == CACHE_FIXUP_BEAM_ATOM_MOV;
        break;
    case CACHE_FIXUP_GLOBAL_FRAGMENT:
    case CACHE_FIXUP_GLOBAL_FRAGMENT_MOV:
        if (beamasm_cache_global_base() == NULL ||
            fixup->target >= beamasm_cache_global_size() ||
            fixup->target > UINTPTR_MAX -
                                    (uintptr_t)beamasm_cache_global_base()) {
            return 0;
        }
        value = (uintptr_t)beamasm_cache_global_base() + fixup->target;
        mov = fixup->kind == CACHE_FIXUP_GLOBAL_FRAGMENT_MOV;
        break;
    case CACHE_FIXUP_RUNTIME_FUNCTION:
    case CACHE_FIXUP_RUNTIME_FUNCTION_MOV:
        value = (uintptr_t)beamasm_cache_runtime_anchor() + fixup->target;
        mov = fixup->kind == CACHE_FIXUP_RUNTIME_FUNCTION_MOV;
        break;
    default:
        return 0;
    }
    if (mov) {
        return fixup->addend == 16 &&
               cache_patch_aarch64_mov(stp, fixup->offset, value);
    }
    return cache_patch_word(stp, fixup->offset, fixup->addend, value);
}

static int cache_patch_fixups(LoaderState *stp,
                              Uint32 kind,
                              Uint32 index,
                              UWord value) {
    Uint i;

    for (i = 0; i < stp->jit_cache_fixup_count; i++) {
        BeamJitCachedFixup *fixup = &stp->jit_cache_fixups[i];
        if (fixup->kind < kind ||
            (fixup->kind == kind && fixup->index < index)) {
            continue;
        }
        if (fixup->kind > kind || fixup->index > index) {
            break;
        }
        if (!cache_patch_word(stp, fixup->offset, fixup->addend, value)) {
            return 0;
        }
    }
    return 1;
}

static const byte *cache_named_rodata(LoaderState *stp,
                                      Uint index,
                                      size_t size) {
    Uint64 offset;

    ASSERT(index < 6);
    offset = stp->jit_cache_named_offsets[index];
    if (offset == 0 || offset > stp->loaded_size ||
        size > stp->loaded_size - offset) {
        return NULL;
    }
    return (const byte *)stp->executable_region + offset;
}

static int cache_copy_metadata(LoaderState *stp,
                               const byte *metadata,
                               size_t metadata_size,
                               Uint64 image_size,
                               Uint64 *code_header_offset,
                               Uint64 *code_end_offset,
                               Uint64 *on_load_offset,
                               Uint32 *function_count) {
    const size_t header_size = 48;
    const size_t fixup_size = 28;
    Uint32 label_count, fixup_count, group_count;
    Uint64 tables_size;
    const byte *functions, *labels, *fixups;
    const byte *record, *metadata_end;
    Uint i, group, parsed_fixups, initial_dynamic_count, nif_bytes;
    Uint32 previous_kind;

    if (metadata_size < header_size ||
        sys_memcmp(metadata, "EJMETA03", 8) != 0) {
        return 0;
    }
    *function_count = cache_read_u32(metadata + 8);
    label_count = cache_read_u32(metadata + 12);
    fixup_count = cache_read_u32(metadata + 16);
    group_count = cache_read_u32(metadata + 20);
    *code_header_offset = cache_read_u64(metadata + 24);
    *code_end_offset = cache_read_u64(metadata + 32);
    *on_load_offset = cache_read_u64(metadata + 40);
    if (*function_count != (Uint32)stp->beam.code.function_count ||
        label_count != (Uint32)stp->beam.code.label_count + 1 ||
        *code_header_offset != 0 || *code_end_offset > image_size ||
        *code_end_offset < sizeof(BeamCodeHeader) ||
        (Uint64)*function_count > (SIZE_MAX - header_size) / 8 ||
        label_count > (SIZE_MAX - header_size - (size_t)*function_count * 8) /
                              8) {
        return 0;
    }
    tables_size = header_size + (Uint64)*function_count * 8 +
                  (Uint64)label_count * 8;
    if (tables_size > metadata_size ||
        group_count > CACHE_FIXUP_LAMBDA_TRAMPOLINE ||
        (fixup_count == 0) != (group_count == 0)) {
        return 0;
    }
    functions = metadata + header_size;
    labels = functions + (size_t)*function_count * 8;
    fixups = labels + (size_t)label_count * 8;
    metadata_end = metadata + metadata_size;

    record = fixups;
    parsed_fixups = 0;
    previous_kind = 0;
    for (group = 0; group < group_count; group++) {
        Uint32 kind, count;
        if ((size_t)(metadata_end - record) < 8) return 0;
        kind = cache_read_u32(record);
        count = cache_read_u32(record + 4);
        record += 8;
        if (kind <= previous_kind || kind > CACHE_FIXUP_LAMBDA_TRAMPOLINE ||
            count == 0 || count > fixup_count - parsed_fixups ||
            count > (size_t)(metadata_end - record) / fixup_size) {
            return 0;
        }
        previous_kind = kind;
        parsed_fixups += count;
        record += (size_t)count * fixup_size;
    }
    if (parsed_fixups != fixup_count ||
        (size_t)(metadata_end - record) < 24 ||
        sys_memcmp(record, "EJLDR001", 8) != 0 ||
        cache_read_u32(record + 12) != (Uint32)stp->beam.lambdas.count ||
        cache_read_u32(record + 16) !=
                ((Uint32)*function_count + 7) / 8 ||
        cache_read_u32(record + 20) != 0) {
        return 0;
    }
    initial_dynamic_count = stp->beam.dynamic_literals.count;
    {
        Uint dynamic_count = cache_read_u32(record + 8);
        if (dynamic_count < initial_dynamic_count) return 0;
        record += 24;
        for (i = 0; i < dynamic_count; i++) {
            Uint32 kind, size;
            Sint literal;
            if ((size_t)(metadata_end - record) < 8) return 0;
            kind = cache_read_u32(record);
            size = cache_read_u32(record + 4);
            record += 8;
            if (size > (size_t)(metadata_end - record)) return 0;
            if (kind > (Uint32)stp->beam.lambdas.count ||
                (kind != 0 && size != 0)) {
                return 0;
            }
            if (i >= initial_dynamic_count) {
                literal = kind == 0
                                  ? beamfile_add_encoded_literal(&stp->beam,
                                                                 record,
                                                                 size)
                                  : beamfile_add_lambda_literal(
                                            &stp->beam,
                                            stp->beam.lambdas.entries[kind - 1]
                                                    .arity);
                if (literal != ~(Sint)i) return 0;
            }
            record += size;
        }
    }
    if ((size_t)(metadata_end - record) <
        (size_t)stp->beam.lambdas.count * 8) {
        return 0;
    }
    for (i = 0; i < (Uint)stp->beam.lambdas.count; i++) {
        stp->lambda_literals[i] = (Sint64)cache_read_u64(record);
        if (stp->beam.lambdas.entries[i].num_free == 0) {
            Sint literal = stp->lambda_literals[i];
            if (literal == ERTS_SWORD_MAX ||
                (literal >= 0
                         ? literal >= stp->beam.static_literals.count
                         : ~literal >= stp->beam.dynamic_literals.count)) {
                return 0;
            }
        }
        record += 8;
    }
    nif_bytes = (*function_count + 7) / 8;
    if ((size_t)(metadata_end - record) != nif_bytes) return 0;
    if (nif_bytes && (*function_count & 7) &&
        (record[nif_bytes - 1] & (0xff << (*function_count & 7)))) {
        return 0;
    }
    for (i = 0; i < *function_count; i++) {
        if (record[i / 8] & (1 << (i & 7))) {
            if (stp->load_hdr->are_nifs == NULL) {
                stp->load_hdr->are_nifs =
                        erts_alloc(ERTS_ALC_T_PREPARED_CODE, *function_count);
                sys_memzero(stp->load_hdr->are_nifs, *function_count);
            }
            stp->load_hdr->are_nifs[i] = 1;
        }
    }
    stp->jit_cache_labels = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                       label_count * sizeof(Uint64));
    stp->jit_cache_label_count = label_count;
    for (i = 0; i < label_count; i++) {
        Uint64 offset = cache_read_u64(labels + i * 8);
        if (offset != UINT64_MAX && offset >= *code_end_offset) {
            return 0;
        }
        stp->jit_cache_labels[i] = offset;
    }

    stp->jit_cache_fixups = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                       fixup_count * sizeof(BeamJitCachedFixup));
    stp->jit_cache_fixup_count = fixup_count;
    stp->jit_cache_lambda_trampolines =
            stp->beam.lambdas.count
                    ? erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                 stp->beam.lambdas.count * sizeof(Uint64))
                    : NULL;
    for (i = 0; i < (Uint)stp->beam.lambdas.count; i++) {
        stp->jit_cache_lambda_trampolines[i] = UINT64_MAX;
    }
    record = fixups;
    i = 0;
    for (group = 0; group < group_count; group++) {
        Uint32 kind = cache_read_u32(record);
        Uint32 count = cache_read_u32(record + 4);
        Uint end = i + count;
        record += 8;
        for (; i < end; i++, record += fixup_size) {
            BeamJitCachedFixup *out = &stp->jit_cache_fixups[i];
            out->kind = kind;
            out->index = cache_read_u32(record);
            out->offset = cache_read_u64(record + 4);
            out->addend = (Sint64)cache_read_u64(record + 12);
            out->target = cache_read_u64(record + 20);
            if (out->offset >= image_size) {
                return 0;
            }
            switch (out->kind) {
            case CACHE_FIXUP_IMPORT:
                if (out->index >= (Uint32)stp->beam.imports.count) return 0;
                break;
            case CACHE_FIXUP_LAMBDA:
            case CACHE_FIXUP_LAMBDA_TRAMPOLINE:
                if (out->index >= (Uint32)stp->beam.lambdas.count) return 0;
                break;
            case CACHE_FIXUP_LITERAL:
                if (((Sint32)out->index >= 0 &&
                     (Sint32)out->index >= stp->beam.static_literals.count) ||
                    ((Sint32)out->index < 0 &&
                     ~((Sint32)out->index) >=
                             stp->beam.dynamic_literals.count)) {
                    return 0;
                }
                break;
            case CACHE_FIXUP_BEAM_ATOM:
            case CACHE_FIXUP_BEAM_ATOM_MOV:
                if (out->index >= (Uint32)stp->beam.atoms.count) return 0;
                break;
            case CACHE_FIXUP_STRING:
                if (out->addend < 0 ||
                    (Uint64)out->addend > stp->beam.strings.size)
                    return 0;
                break;
            default:
                break;
            }
            if ((out->kind == CACHE_FIXUP_BEAM_ATOM_MOV ||
                 out->kind == CACHE_FIXUP_GLOBAL_FRAGMENT_MOV ||
                 out->kind == CACHE_FIXUP_RUNTIME_FUNCTION_MOV)
                        ? (image_size < 16 || out->offset > image_size - 16)
                        : (out->kind <= CACHE_FIXUP_RUNTIME_FUNCTION &&
                           (image_size < sizeof(UWord) ||
                            out->offset > image_size - sizeof(UWord)))) {
                return 0;
            }
            if (out->kind == CACHE_FIXUP_CATCH && out->target >= image_size) {
                return 0;
            }
            if (out->kind == CACHE_FIXUP_NAMED_OFFSET) {
                if (out->index >= 6) return 0;
                stp->jit_cache_named_offsets[out->index] = out->offset;
            } else if (out->kind == CACHE_FIXUP_LAMBDA_TRAMPOLINE) {
                stp->jit_cache_lambda_trampolines[out->index] = out->offset;
            }
        }
    }
    for (i = 0; i < *function_count; i++) {
        Uint64 offset = cache_read_u64(functions + i * 8);
        if ((offset & (sizeof(UWord) - 1)) != 0 ||
            offset < offsetof(BeamCodeHeader, functions) ||
            offset > *code_end_offset - sizeof(ErtsCodeInfo)) {
            return 0;
        }
    }
    return 1;
}

int beam_load_cache_image(LoaderState *stp,
                          const byte *cache_data,
                          size_t cache_size) {
#if defined(__aarch64__)
    BeamJitCache cache;
    Uint64 metadata_offset, code_header_offset, code_end_offset, on_load_offset;
    Uint32 function_count;
    const byte *metadata;
    size_t metadata_size, code_header_size;
    const void *exec;
    void *rw;
    BeamCodeHeader *header;
    Uint i;
    ErtsSysHrTime phase_start = erts_beam_load_timing_start();

    if (!beam_jit_cache_open(cache_data, cache_size, &cache) ||
        cache.image_size > UINT_MAX ||
        cache.image + cache.image_size < cache.image) {
        return 0;
    }
    metadata_offset = (Uint64)(cache.image - cache.data) + cache.image_size;
    if (metadata_offset > cache_size) return 0;
    metadata = cache_data + metadata_offset;
    metadata_size = cache_size - (size_t)metadata_offset;
    if (!cache_copy_metadata(stp,
                             metadata,
                             metadata_size,
                             cache.image_size,
                             &code_header_offset,
                             &code_end_offset,
                             &on_load_offset,
                             &function_count)) {
        return 0;
    }
    if (on_load_offset != UINT64_MAX && on_load_offset >= code_end_offset) {
        return 0;
    }
    for (i = 0; i < (Uint)stp->beam.exports.count; i++) {
        Uint label = stp->beam.exports.entries[i].label;
        if (label >= stp->jit_cache_label_count ||
            stp->jit_cache_labels[label] == UINT64_MAX) {
            return 0;
        }
    }
    erts_beam_load_timing_report("cache_parse", phase_start);
    phase_start = erts_beam_load_timing_start();
    if (!beamasm_install_cache_code(cache.image,
                                    (size_t)cache.image_size,
                                    &exec,
                                    &rw)) {
        return 0;
    }
    erts_beam_load_timing_report("cache_copy", phase_start);
    stp->executable_region = exec;
    stp->writable_region = rw;
    stp->loaded_size = (unsigned)cache.image_size;
    phase_start = erts_beam_load_timing_start();
    if (!beam_jit_cache_load_aarch64(&cache,
                                     rw,
                                     exec,
                                     stp->loaded_size,
                                     NULL,
                                     NULL)) {
        beamasm_purge_module(exec, rw, stp->loaded_size);
        stp->executable_region = NULL;
        stp->writable_region = NULL;
        return 0;
    }
    for (i = 0; i < stp->jit_cache_fixup_count; i++) {
        BeamJitCachedFixup *fixup = &stp->jit_cache_fixups[i];
        if ((fixup->kind >= CACHE_FIXUP_BEAM_ATOM &&
             fixup->kind <= CACHE_FIXUP_RUNTIME_FUNCTION) ||
            (fixup->kind >= CACHE_FIXUP_BEAM_ATOM_MOV &&
             fixup->kind <= CACHE_FIXUP_RUNTIME_FUNCTION_MOV)) {
            if (!cache_apply_immediate_fixup(stp, fixup)) return 0;
        }
    }
    erts_beam_load_timing_report("cache_relocate", phase_start);
    phase_start = erts_beam_load_timing_start();
    code_header_size = offsetof(BeamCodeHeader, functions) +
                       sizeof(BeamInstr) * stp->codev_size;
    if (code_header_size > code_end_offset) return 0;
    header = (BeamCodeHeader *)rw;
    sys_memcpy(header, stp->load_hdr, code_header_size);
    for (i = 0; i < function_count; i++) {
        Uint64 offset = cache_read_u64(metadata + 48 + i * 8);
        header->functions[i] = (const ErtsCodeInfo *)((const byte *)exec +
                                                      offset);
    }
    header->functions[function_count] =
            (const ErtsCodeInfo *)((const byte *)exec + code_end_offset);
    header->on_load = on_load_offset == UINT64_MAX
                              ? NULL
                              : (const ErtsCodeInfo *)((const byte *)exec +
                                                       on_load_offset);
    stp->code_hdr = (const BeamCodeHeader *)exec;
    stp->on_load = header->on_load;
    stp->jit_cache_loaded = 1;
    if (!beam_load_finish_cache(stp)) {
        return 0;
    }
    beamasm_seal_module(exec, rw, stp->loaded_size);
    erts_beam_load_timing_report("cache_finalize", phase_start);
    return 1;
#else
    (void)stp;
    (void)cache_data;
    (void)cache_size;
    return 0;
#endif
}

#if 0
static int cache_is_identity_module(LoaderState *stp) {
    static const int expected[] = {genop_int_func_start_5,
                                   genop_label_1,
                                   genop_return_0,
                                   genop_int_func_end_2,
                                   genop_int_code_end_0};
    BeamCodeReader *reader;
    BeamOp *op = NULL;
    int i;

    reader = beamfile_get_code(&stp->beam, &stp->op_allocator);
    for (i = 0; i < sizeof(expected) / sizeof(expected[0]); i++) {
        if (!beamcodereader_next(reader, &op) || op->op != expected[i]) {
            beamcodereader_close(reader);
            return 0;
        }
    }
    beamcodereader_close(reader);
    return 1;
}

static int cache_is_identity_aarch64(const byte *code, size_t size) {
    static const Uint32 expected[] = {0xf81f8e9e,
                                      0x14000002,
                                      0x94000000,
                                      0x10ffffa2,
                                      0x710006d6,
                                      0x5400000d,
                                      0xf840869e,
                                      0x710006d6,
                                      0x54000004,
                                      0xd65f03c0};
    Uint32 instruction;
    Uint i;

    if (size != sizeof(expected)) {
        return 0;
    }
    for (i = 0; i < sizeof(expected) / sizeof(expected[0]); i++) {
        sys_memcpy(&instruction, code + i * sizeof(instruction),
                   sizeof(instruction));
        if (i == 2) {
            instruction &= 0xfc000000;
        } else if (i == 5 || i == 8) {
            instruction &= 0xff00001f;
        }
        if (instruction != expected[i]) {
            return 0;
        }
    }
    return 1;
}

static int patch_aarch64_b_cond(void *rw,
                                const void *exec,
                                Uint64 offset,
                                ErtsCodePtr target) {
    Sint64 delta = (const byte *)target - ((const byte *)exec + offset);
    Uint32 instruction;

    if ((delta & 3) != 0 || delta < -(Sint64)(1 << 20) ||
        delta >= (Sint64)(1 << 20)) {
        return 0;
    }
    sys_memcpy(&instruction, (byte *)rw + offset, sizeof(instruction));
    instruction = (instruction & 0xff00001f) |
                  ((((Uint32)(delta / 4)) & 0x7ffff) << 5);
    sys_memcpy((byte *)rw + offset, &instruction, sizeof(instruction));
    return 1;
}

static int patch_aarch64_bl(void *rw,
                            const void *exec,
                            Uint64 offset,
                            ErtsCodePtr target) {
    Sint64 delta = (const byte *)target - ((const byte *)exec + offset);
    Uint32 branch;

    if ((delta & 3) != 0 || delta < -(Sint64)(1 << 27) ||
        delta >= (Sint64)(1 << 27)) {
        return 0;
    }
    branch = 0x94000000u | (((Uint32)(delta / 4)) & 0x03ffffffu);
    sys_memcpy((byte *)rw + offset, &branch, sizeof(branch));
    return 1;
}

static void emit_aarch64_absolute_jump(void *rw,
                                       Uint64 offset,
                                       ErtsCodePtr target) {
    const UWord address = (UWord)target;
    Uint32 code[] = {0xd2800010u | (Uint32)((address & 0xffff) << 5),
                     0xf2a00010u | (Uint32)(((address >> 16) & 0xffff) << 5),
                     0xf2c00010u | (Uint32)(((address >> 32) & 0xffff) << 5),
                     0xf2e00010u | (Uint32)(((address >> 48) & 0xffff) << 5),
                     0xd61f0200u};

    sys_memcpy((byte *)rw + offset, code, sizeof(code));
}

int beam_load_cache_image(LoaderState *stp,
                          const byte *cache,
                          size_t cache_size) {
#if defined(__aarch64__)
    const size_t header_size = 80;
    const size_t section_record_size = 40;
    const size_t reloc_record_size = 64;
    Uint32 section_count, reloc_count, function_count, label_count;
    Uint64 image_size, sections_offset, relocs_offset, image_offset;
    Uint64 metadata_offset, code_header_offset, code_end_offset;
    Uint64 on_load_offset, function_offset;
    size_t metadata_size, code_header_size;
    const byte *metadata, *label_data;
    const void *exec;
    void *rw;
    BeamCodeHeader *code_hdr_rw;
    const BeamCodeHeader *code_hdr_exec;
    const ErtsCodeInfo **function_table;
    BeamFile_ExportEntry *entry;
    Uint64 entry_offset;
    Uint i;

    if (cache_size < header_size ||
        sys_memcmp(cache, "EJITC001", 8) != 0 ||
        cache_read_u32(cache + 8) != 1 ||
        cache_read_u32(cache + 12) != header_size ||
        cache_read_u32(cache + 16) != 1) {
        return 0;
    }

    section_count = cache_read_u32(cache + 20);
    reloc_count = cache_read_u32(cache + 24);
    image_size = cache_read_u64(cache + 32);
    sections_offset = cache_read_u64(cache + 56);
    relocs_offset = cache_read_u64(cache + 64);
    image_offset = cache_read_u64(cache + 72);

    if (section_count == 0 || reloc_count != 0 ||
        sections_offset != header_size ||
        section_count > (SIZE_MAX - header_size) / section_record_size ||
        relocs_offset != header_size +
                                  (size_t)section_count * section_record_size ||
        image_offset != relocs_offset +
                                (size_t)reloc_count * reloc_record_size ||
        image_size > SIZE_MAX - image_offset ||
        image_offset + image_size > cache_size) {
        return 0;
    }

    metadata_offset = image_offset + image_size;
    if (metadata_offset > cache_size || cache_size - metadata_offset < 40) {
        return 0;
    }
    metadata = cache + metadata_offset;
    if (sys_memcmp(metadata, "EJMETA01", 8) != 0) {
        return 0;
    }

    function_count = cache_read_u32(metadata + 8);
    label_count = cache_read_u32(metadata + 12);
    code_header_offset = cache_read_u64(metadata + 16);
    code_end_offset = cache_read_u64(metadata + 24);
    on_load_offset = cache_read_u64(metadata + 32);
    metadata_size = 40 + (size_t)function_count * 8 +
                    (size_t)label_count * 8;

    if (function_count != 1 || stp->beam.code.function_count != 1 ||
        label_count != stp->beam.code.label_count + 1 ||
        metadata_size != cache_size - metadata_offset ||
        code_header_offset != 0 || code_end_offset > image_size ||
        code_end_offset < sizeof(BeamCodeHeader) || image_size > UINT_MAX ||
        image_size - code_end_offset < 10 * sizeof(Uint32) ||
        on_load_offset != UINT64_MAX || stp->beam.exports.count != 1 ||
        stp->beam.imports.count != 0 || stp->beam.lambdas.count != 0 ||
        stp->beam.static_literals.count != 0 ||
        stp->beam.dynamic_literals.count > 1 || stp->beam.strings.size != 0 ||
        stp->beam.debug.item_count != 0 || stp->beam.record.record_count != 0 ||
        stp->coverage != NULL || !cache_is_identity_module(stp)) {
        return 0;
    }

    function_offset = cache_read_u64(metadata + 40);
    label_data = metadata + 40 + function_count * 8;
    entry = &stp->beam.exports.entries[0];
    code_header_size = offsetof(BeamCodeHeader, functions) +
                       sizeof(BeamInstr) * stp->codev_size;
    if (entry->label >= label_count) {
        return 0;
    }
    entry_offset = cache_read_u64(label_data + entry->label * 8);
    if ((function_offset & (sizeof(UWord) - 1)) != 0 ||
        function_offset < code_header_size ||
        function_offset > code_end_offset - sizeof(ErtsCodeInfo) ||
        entry_offset != function_offset + sizeof(ErtsCodeInfo) ||
        entry_offset >= code_end_offset ||
        !cache_is_identity_aarch64(cache + image_offset + entry_offset,
                                   code_end_offset - entry_offset) ||
        code_header_size > code_end_offset) {
        return 0;
    }

    if (!beamasm_install_cache_code(cache + image_offset,
                                    (size_t)image_size,
                                    &exec,
                                    &rw)) {
        return 0;
    }

    stp->executable_region = exec;
    stp->writable_region = rw;
    stp->loaded_size = (unsigned)image_size;
    sys_memset((byte *)rw + code_end_offset,
               0,
               (size_t)(image_size - code_end_offset));
    stp->jit_cache_labels = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                       label_count * sizeof(Uint64));
    stp->jit_cache_label_count = label_count;
    for (i = 0; i < label_count; i++) {
        stp->jit_cache_labels[i] = cache_read_u64(label_data + i * 8);
    }

    code_hdr_rw = (BeamCodeHeader *)rw;
    code_hdr_exec = (const BeamCodeHeader *)exec;
    sys_memcpy(code_hdr_rw, stp->load_hdr, code_header_size);
    code_hdr_rw->on_load = NULL;
    function_table = &code_hdr_rw->functions[0];
    function_table[0] =
            (ErtsCodeInfo *)((const byte *)exec + function_offset);
    function_table[1] =
            (ErtsCodeInfo *)((const byte *)exec + code_end_offset);

    {
        ErtsCodeInfo *info =
                (ErtsCodeInfo *)((byte *)rw + function_offset);
        info->gen_bp = NULL;
        info->mfa.module = stp->module;
        info->mfa.function = entry->function;
        info->mfa.arity = entry->arity;
    }

    if (!patch_aarch64_bl(rw,
                          exec,
                          function_offset,
                          beamasm_i_func_info_shared()) ||
        !patch_aarch64_bl(rw,
                          exec,
                          entry_offset + 2 * sizeof(Uint32),
                          beamasm_i_breakpoint_trampoline_shared())) {
        return 0;
    }
    emit_aarch64_absolute_jump(rw,
                               code_end_offset,
                               beamasm_i_test_yield_shared());
    emit_aarch64_absolute_jump(rw,
                               code_end_offset + 5 * sizeof(Uint32),
                               beamasm_dispatch_return());
    if (!patch_aarch64_b_cond(rw,
                              exec,
                              entry_offset + 5 * sizeof(Uint32),
                              (ErtsCodePtr)((const byte *)exec +
                                            code_end_offset)) ||
        !patch_aarch64_b_cond(rw,
                              exec,
                              entry_offset + 8 * sizeof(Uint32),
                              (ErtsCodePtr)((const byte *)exec +
                                            code_end_offset +
                                            5 * sizeof(Uint32)))) {
        return 0;
    }

    stp->code_hdr = code_hdr_exec;
    stp->on_load = NULL;
    stp->jit_cache_loaded = 1;
    beamasm_seal_module(stp->executable_region,
                        stp->writable_region,
                        stp->loaded_size);
    return 1;
#else
    (void)stp;
    (void)cache;
    (void)cache_size;
    return 0;
#endif
}
#endif

static void init_label(Label *lp) {
    sys_memset(lp, 0, sizeof(*lp));
    lp->lambda_index = INVALID_LAMBDA_INDEX;
}

void beam_load_prepared_free(Binary *magic) {
    beam_load_prepared_dtor(magic);
    erts_bin_release(magic);
}

/* This destructor function can safely be called multiple times. */
int beam_load_prepared_dtor(Binary *magic) {
    LoaderState *stp = ERTS_MAGIC_BIN_DATA(magic);

    /* This should have been freed earlier! */
    ASSERT(stp->op_allocator.beamop_blocks == NULL);

    beamfile_free(&stp->beam);
    beamopallocator_dtor(&stp->op_allocator);

    if (stp->load_hdr) {
        BeamCodeHeader *hdr = stp->load_hdr;

        if (hdr->literal_area) {
            erts_release_literal_area(hdr->literal_area);
            hdr->literal_area = NULL;
        }
        if (hdr->are_nifs) {
            erts_free(ERTS_ALC_T_PREPARED_CODE, hdr->are_nifs);
            hdr->are_nifs = NULL;
        }
        if (hdr->coverage) {
            erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->coverage);
            hdr->coverage = NULL;
        }
        if (hdr->line_coverage_valid) {
            erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->line_coverage_valid);
            hdr->line_coverage_valid = NULL;
        }

        if (hdr->loc_index_to_cover_id) {
            erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->loc_index_to_cover_id);
            hdr->loc_index_to_cover_id = NULL;
        }

        erts_free(ERTS_ALC_T_PREPARED_CODE, hdr);
        stp->load_hdr = NULL;
    }

    if (stp->labels) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *)stp->labels);
        stp->labels = NULL;
    }

    if (stp->jit_cache_labels) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->jit_cache_labels);
        stp->jit_cache_labels = NULL;
        stp->jit_cache_label_count = 0;
    }

    if (stp->jit_cache_fixups) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->jit_cache_fixups);
        stp->jit_cache_fixups = NULL;
        stp->jit_cache_fixup_count = 0;
    }

    if (stp->jit_cache_lambda_trampolines) {
        erts_free(ERTS_ALC_T_PREPARED_CODE,
                  stp->jit_cache_lambda_trampolines);
        stp->jit_cache_lambda_trampolines = NULL;
    }

    if (stp->lambda_literals) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *)stp->lambda_literals);
        stp->lambda_literals = NULL;
    }

    if (stp->bif_imports) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->bif_imports);
        stp->bif_imports = NULL;
    }

    if (stp->line_instr) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->line_instr);
        stp->line_instr = NULL;
    }

    if (stp->func_line) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->func_line);
        stp->func_line = NULL;
    }

    if (stp->coverage) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, stp->coverage);
        stp->coverage = NULL;
    }

    if (stp->line_coverage_valid) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, stp->line_coverage_valid);
        stp->line_coverage_valid = NULL;
    }

    if (stp->loc_index_to_cover_id) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, stp->loc_index_to_cover_id);
        stp->loc_index_to_cover_id = NULL;
    }

    if (stp->ba) {
        beamasm_delete_assembler(stp->ba);
        stp->ba = NULL;
    }

    if (stp->executable_region) {
        ASSERT(stp->writable_region != NULL);

        beamasm_purge_module(stp->executable_region,
                             stp->writable_region,
                             stp->loaded_size);

        stp->executable_region = NULL;
        stp->writable_region = NULL;
    }

    return 1;
}

static int add_line_entry(LoaderState *stp,
                          BeamInstr item,
                          int insert_duplicates) {
    int is_duplicate;
    unsigned int li;

    if (!stp->beam.lines.item_count) {
        return 0;
    }

    if (item >= stp->beam.lines.item_count) {
        BeamLoadError2(stp,
                       "line instruction index overflow (%u/%u)",
                       item,
                       stp->beam.lines.item_count);
    }

    li = stp->current_li;
    if (li >= stp->beam.lines.instruction_count) {
        BeamLoadError2(stp,
                       "line instruction table overflow (%u/%u)",
                       li,
                       stp->beam.lines.instruction_count);
    }

    is_duplicate = li && (stp->line_instr[li - 1].loc == item);

    if (insert_duplicates || !is_duplicate) {
        stp->line_instr[li].pos = stp->jit_cache_metadata_only
                                          ? 0
                                          : beamasm_get_offset(stp->ba);
        stp->line_instr[li].loc = item;
        stp->current_li++;
    }

    return 0;

load_error:
    return -1;
}

int beam_load_emit_op(LoaderState *stp, BeamOp *tmp_op) {
    const char *sign;
    int arg;


    /*
     * Verify and massage the operands for the specific instruction.
     *
     * After the massaging, TAG_i denotes a tagged immediate value,
     * either NIL, an atom, or a small integer. The tags TAG_n and
     * TAG_a are no longer used. TAG_f can either be a non-zero label
     * number (in guards) or 0 (in function bodies) to indicate that
     * an exception should be generated on failure. TAG_j is no longer
     * used.
     */
    sign = opc[stp->specific_op].sign;
    arg = 0;
    ASSERT(sign != NULL);
    while (*sign) {
        Uint tag;
        BeamOpArg *curr = &stp->genop->a[arg];

        ASSERT(arg < stp->genop->arity);
        tag = curr->type;

        switch (*sign) {
        case 'n': /* Nil */
            ASSERT(tag != TAG_r);
            curr->type = 'I';
            curr->val = NIL;
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'x': /* x(N) */
        case 'y': /* y(N) */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'a': /* Tagged atom */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            curr->type = 'I';
            break;
        case 'c': /* Tagged constant */
            switch (tag) {
            case TAG_i:
                curr->val = make_small((Uint)curr->val);
                curr->type = 'I';
                break;
            case TAG_a:
                curr->type = 'I';
                break;
            case TAG_n:
                curr->val = NIL;
                curr->type = 'I';
                break;
            case TAG_q:
                break;
            default:
                BeamLoadError1(stp,
                               "bad tag %d for tagged constant",
                               curr->type);
                break;
            }
            break;
        case 's':
            /* Any source (tagged constant or register) */
            switch (tag) {
            case TAG_x:
                break;
            case TAG_y:
                break;
            case TAG_i:
                curr->val = (BeamInstr)make_small(curr->val);
                curr->type = 'I';
                break;
            case TAG_a:
                curr->type = 'I';
                break;
            case TAG_n:
                curr->type = 'I';
                curr->val = NIL;
                break;
            case TAG_q: {
                Eterm term = beamfile_get_literal(&stp->beam, curr->val);
                switch (loader_tag(term)) {
                case LOADER_X_REG:
                case LOADER_Y_REG:
                    BeamLoadError1(stp,
                                   "the term '%T' would be confused "
                                   "with a register",
                                   term);
                }
            } break;
            default:
                BeamLoadError1(stp,
                               "bad tag %d for general source",
                               curr->type);
                break;
            }
            break;
        case 'd': /* Destination (x(N), y(N) */
        case 'S': /* Source (x(N), y(N)) */
            switch (tag) {
            case TAG_x:
                break;
            case TAG_y:
                break;
            default:
                BeamLoadError1(stp, "bad tag %d for destination", curr->type);
                break;
            }
            break;
        case 't': /* Small untagged integer (16 bits) -- can be packed. */
        case 'I': /* Untagged integer (32 bits) -- can be packed.  */
        case 'W': /* Untagged integer or pointer (machine word). */
#ifdef DEBUG
            switch (*sign) {
            case 't':
                /* 't'-typed values must fit in 16 bits. */
                ASSERT((curr->val >> 16) == 0);
                break;
#    ifdef ARCH_64
            case 'I':
            case 'V':
                /* 'I'- and 'V'-typed values must fit in 32 bits. */
                ASSERT((curr->val >> 32) == 0);
                break;
#    endif
            }
#endif
            BeamLoadVerifyTag(stp, tag, TAG_u);
            break;
        case 'A': /* Arity value. */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            curr->val = curr->val == 0 ? make_arityval_zero()
                                       : make_arityval(curr->val);
            break;
        case 'f': /* Destination label */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'j': /* 'f' or 'p' */
            switch (tag) {
            case TAG_p:
                curr->type = TAG_f;
                curr->val = 0;
                break;
            case TAG_f:
                break;
            default:
                BeamLoadError3(stp,
                               "bad tag %d; expected %d or %d",
                               tag,
                               TAG_f,
                               TAG_p);
            }

            break;
        case 'L': /* Define label */
            ASSERT(stp->specific_op == op_label_L ||
                   stp->specific_op == op_aligned_label_Lt ||
                   stp->specific_op == op_i_func_label_L);
            BeamLoadVerifyTag(stp, tag, TAG_u);
            stp->last_label = curr->val;
            if (stp->last_label < 0 ||
                stp->last_label >= stp->beam.code.label_count) {
                BeamLoadError2(stp,
                               "invalid label num %u (0 < label < %u)",
                               curr->val,
                               stp->beam.code.label_count);
            }
            if (stp->labels[stp->last_label].value != 0) {
                BeamLoadError1(stp,
                               "label %d defined more than once",
                               stp->last_label);
            }
            stp->labels[stp->last_label].value = 1;
            curr->type = TAG_f;
            break;
        case 'e': /* Export entry */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            if (curr->val >= stp->beam.imports.count) {
                BeamLoadError1(stp, "invalid import table index %d", curr->val);
            }
            curr->type = 'E';
            break;
        case 'b': {
            int i = tmp_op->a[arg].val;
            BeamLoadVerifyTag(stp, tag, TAG_u);
            if (i >= stp->beam.imports.count) {
                BeamLoadError1(stp, "invalid import table index %d", i);
            } else if (stp->bif_imports[i] == NULL) {
                BeamLoadError1(stp, "import %d not a BIF", i);
            } else {
                curr->val = (BeamInstr)stp->bif_imports[i]->f;
            }
        } break;
        case 'P': /* Byte offset into tuple */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            curr->val = (BeamInstr)((curr->val + 1) * sizeof(Eterm));
            break;
        case 'l': /* Floating point register. */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            curr->val = curr->val * sizeof(FloatDef);
            break;
        case 'q': /* Literal */
            BeamLoadVerifyTag(stp, tag, TAG_q);
            break;
        case 'F': /* Fun entry */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            curr->type = 'F';
            break;
        case 'H': /* Exception handler */
            BeamLoadVerifyTag(stp, tag, TAG_f);
            curr->type = 'H';
            break;
        case 'M':
            curr->type = 'M';
            break;
        case 'i':
            curr->type = 'I';
            break;
        default:
            BeamLoadError1(stp, "bad argument tag: %d", *sign);
        }

        /* These types must have been translated to 'I' */
        ASSERT(curr->type != TAG_i && curr->type != TAG_n &&
               curr->type != TAG_a && curr->type != TAG_v);

        sign++;
        arg++;
    }

    /*
     * Verify and massage any list arguments according to the primitive tags.
     *
     * 'I' will denote a tagged immediate value (NIL, small integer,
     * atom, or tuple arity). TAG_n, TAG_a, and TAG_v will no longer be used.
     */
    for (; arg < tmp_op->arity; arg++) {
        BeamOpArg *curr = &tmp_op->a[arg];

        switch (tmp_op->a[arg].type) {
        case TAG_i:
            curr->val = make_small(tmp_op->a[arg].val);
            curr->type = 'I';
            break;
        case TAG_n:
            curr->val = NIL;
            curr->type = 'I';
            break;
        case TAG_a:
        case TAG_v:
            curr->type = 'I';
            break;
        case TAG_u:
        case TAG_f:
        case TAG_x:
        case TAG_y:
        case TAG_q:
            break;
        default:
            BeamLoadError1(stp,
                           "unsupported primitive type '%c'",
                           tag_to_letter[tmp_op->a[arg].type]);
        }
    }

    /* Handle a few special cases. */
    switch (stp->specific_op) {
    case op_i_func_info_IaaI:
        if (stp->function_number >= stp->beam.code.function_count) {
            BeamLoadError1(stp,
                           "too many functions in module (header said %u)",
                           stp->beam.code.function_count);
        }

        stp->function_number++;

        /* Save context for error messages. */
        stp->function = tmp_op->a[2].val;
        stp->arity = tmp_op->a[3].val;

        if (stp->arity > MAX_ARG) {
            BeamLoadError1(stp, "too many arguments: %d", stp->arity);
        }

        break;
    case op_empty_func_line:
    case op_func_line_I:
        /* This is the first line instruction of a function, preceding
         * the func_info instruction. */
        if (stp->func_line) {
            stp->func_line[stp->function_number] = stp->current_li;
        }

        break;
    case op_nif_start:
        if (!stp->load_hdr->are_nifs) {
            int bytes = stp->beam.code.function_count * sizeof(byte);
            stp->load_hdr->are_nifs =
                    erts_alloc(ERTS_ALC_T_PREPARED_CODE, bytes);
            sys_memzero(stp->load_hdr->are_nifs, bytes);
        }
        ASSERT(stp->function_number > 0);
        ASSERT(stp->function_number <= stp->beam.code.function_count);
        stp->load_hdr->are_nifs[stp->function_number - 1] = 1;
        break;
    }

    /* Generate assembly code for the specific instruction. */
    if (!stp->jit_cache_metadata_only) {
        stp->emitted_ops++;
        if (beamasm_emit(stp->ba, stp->specific_op, tmp_op) == 0) {
            BeamLoadError1(stp, "failed to emit asm for %d", stp->specific_op);
        }
    }

    switch (stp->specific_op) {
    case op_func_line_I:
        /* Since this is the beginning of a new function, force insertion
         * of the line entry even if it happens to be a duplicate of the
         * previous one. */
        if (add_line_entry(stp, tmp_op->a[0].val, 1)) {
            goto load_error;
        }
        break;
    case op_line_I:
        /* We'll save some memory by not inserting a line entry that
         * is equal to the previous one. */
        if (add_line_entry(stp, tmp_op->a[0].val, 0)) {
            goto load_error;
        }
        break;
    case op_executable_line_II: {
        byte coverage_size = 0;

        /* We'll save some memory by not inserting a line entry that
         * is equal to the previous one. */
        if (add_line_entry(stp, tmp_op->a[0].val, 0)) {
            goto load_error;
        }
        if (stp->load_hdr->coverage_mode == ERTS_COV_LINE) {
            coverage_size = sizeof(byte);
        } else if (stp->load_hdr->coverage_mode == ERTS_COV_LINE_COUNTERS) {
            coverage_size = sizeof(Uint);
        }
        if (coverage_size) {
            unsigned loc_index = stp->current_li - 1;
            unsigned cover_id = tmp_op->a[1].val;

            ASSERT(stp->beam.lines.item_count > 0);
            stp->line_coverage_valid[loc_index] = 1;
            if (stp->loc_index_to_cover_id) {
                stp->loc_index_to_cover_id[loc_index] = cover_id;
            }
            beamasm_emit_coverage(stp->ba,
                                  stp->coverage,
                                  loc_index,
                                  coverage_size);
        }
        break;
    }
    case op_i_debug_line_IIt: {
        BeamFile_DebugItem *items = stp->beam.debug.items;
        Uint location_index = tmp_op->a[0].val;
        Sint index = tmp_op->a[1].val - 1;

        if (index >= stp->beam.debug.item_count) {
            BeamLoadError2(stp,
                           "debug_line index %u out of range (only %u entries)",
                           index,
                           stp->beam.debug.item_count);
        }

        /* Each i_debug_line is a distinct instrumentation point and we don't
         * want to miss a single one of them (so they all can be selected),
         * so allow duplicates here.
         */
        if (add_line_entry(stp, location_index, 1)) {
            goto load_error;
        }

        ASSERT(items[index].location_index == -1);
        items[index].location_index = stp->current_li - 1;

        break;
    }
    case op_int_code_end:
        /* End of code found. */
        if (stp->function_number != stp->beam.code.function_count) {
            BeamLoadError2(stp,
                           "too few functions (%u) in module (header said %u)",
                           stp->function_number,
                           stp->beam.code.function_count);
        }

        stp->function = THE_NON_VALUE;
        stp->genop = NULL;
        stp->specific_op = -1;

        if (stp->load_hdr->coverage_mode == ERTS_COV_LINE ||
            stp->load_hdr->coverage_mode == ERTS_COV_LINE_COUNTERS) {
            stp->load_hdr->line_coverage_len = stp->current_li;
        }
        break;
    case op_i_test_yield:
        if (stp->load_hdr->coverage_mode == ERTS_COV_FUNCTION) {
            ASSERT(stp->function_number != 0);
            beamasm_emit_coverage(stp->ba,
                                  stp->coverage,
                                  stp->function_number - 1,
                                  sizeof(byte));
        } else if (stp->load_hdr->coverage_mode == ERTS_COV_FUNCTION_COUNTERS) {
            ASSERT(stp->function_number != 0);
            beamasm_emit_coverage(stp->ba,
                                  stp->coverage,
                                  stp->function_number - 1,
                                  sizeof(Uint));
        }
        break;
    }

    return 1;

load_error:
    return 0;
}

static void *get_writable_ptr(const void *executable,
                              void *writable,
                              const void *ptr) {
    const char *exec_raw;
    const char *ptr_raw;
    char *rw_raw;

    exec_raw = (const char *)executable;
    rw_raw = (char *)writable;

    ptr_raw = (const char *)ptr;

    return (void *)(&rw_raw[ptr_raw - exec_raw]);
}

static const BeamCodeLineTab *finish_line_table(LoaderState *stp,
                                                char *module_base,
                                                size_t module_size) {
    const unsigned int ftab_size = stp->beam.code.function_count;
    const unsigned int num_instrs = stp->current_li;

    const BeamCodeLineTab *line_tab_ro;
    BeamCodeLineTab *line_tab_rw;

    const void **line_items_ro;
    void **line_items_rw;

    const unsigned int num_names = stp->beam.lines.name_count;
    const Eterm *fname_base_ro;
    Eterm *fname_base_rw;

    const void *locp_base_ro;
    void *locp_base_rw;

    int i;

    if (stp->line_instr == 0) {
        return NULL;
    }

    line_tab_ro = stp->jit_cache_loaded
                          ? (const BeamCodeLineTab *)cache_named_rodata(
                                    stp, 4, sizeof(BeamCodeLineTab))
                          : (const BeamCodeLineTab *)beamasm_get_rodata(
                                    stp->ba, "line");
    if (line_tab_ro == NULL) {
        return NULL;
    }
    line_items_ro = (const void **)&line_tab_ro->func_tab[ftab_size + 1];
    fname_base_ro = (Eterm *)&line_items_ro[num_instrs + 1];
    locp_base_ro = &fname_base_ro[stp->beam.lines.name_count];

    line_tab_rw = get_writable_ptr(stp->executable_region,
                                   stp->writable_region,
                                   line_tab_ro);
    line_items_rw = get_writable_ptr(stp->executable_region,
                                     stp->writable_region,
                                     line_items_ro);
    locp_base_rw = get_writable_ptr(stp->executable_region,
                                    stp->writable_region,
                                    locp_base_ro);
    fname_base_rw = get_writable_ptr(stp->executable_region,
                                     stp->writable_region,
                                     fname_base_ro);

    line_tab_rw->loc_size = stp->beam.lines.location_size;
    line_tab_rw->fname_ptr = fname_base_ro;

    for (i = 0; i < ftab_size; i++) {
        line_tab_rw->func_tab[i] = line_items_ro + stp->func_line[i];
    }
    line_tab_rw->func_tab[i] = line_items_ro + num_instrs;

    for (i = 0; i < num_instrs; i++) {
        line_items_rw[i] = (void *)&module_base[stp->line_instr[i].pos];
    }

    line_items_rw[i] = (void *)&module_base[module_size];

    for (i = 0; i < num_names; i++) {
        fname_base_rw[i] =
                beamfile_get_literal(&stp->beam, stp->beam.lines.names[i]);
    }

    if (stp->beam.lines.location_size == sizeof(Uint16)) {
        Uint16 *locp = (Uint16 *)locp_base_rw;
        line_tab_rw->loc_tab.p2 = (Uint16 *)locp_base_ro;

        for (i = 0; i < num_instrs; i++) {
            BeamFile_LineEntry *entry;
            int idx;

            idx = stp->line_instr[i].loc;
            entry = &stp->beam.lines.items[idx];
            *locp++ = MAKE_LOCATION(entry->name_index, entry->location);
        }

        *locp++ = LINE_INVALID_LOCATION;
    } else {
        Uint32 *locp = (Uint32 *)locp_base_rw;
        line_tab_rw->loc_tab.p4 = (Uint32 *)locp_base_ro;

        ASSERT(stp->beam.lines.location_size == sizeof(Uint32));

        for (i = 0; i < num_instrs; i++) {
            BeamFile_LineEntry *entry;
            int idx;

            idx = stp->line_instr[i].loc;
            entry = &stp->beam.lines.items[idx];
            *locp++ = MAKE_LOCATION(entry->name_index, entry->location);
        }

        *locp++ = LINE_INVALID_LOCATION;
    }

    return line_tab_ro;
}

static const BeamDebugTab *finish_debug_table(LoaderState *stp,
                                              char *module_base,
                                              size_t module_size) {
    BeamFile_DebugTable *debug = &stp->beam.debug;
    const BeamDebugTab *debug_tab_ro;
    byte *debug_tab_rw_base;
    BeamDebugTab *debug_tab_top;
    Eterm *debug_tab_terms;
    BeamDebugItem *debug_tab_items;
    Uint item_count = debug->item_count;
    Uint term_count = debug->term_count;
    Uint i;

    if (item_count == 0) {
        return NULL;
    }

    debug_tab_ro = stp->jit_cache_loaded
                           ? (const BeamDebugTab *)cache_named_rodata(
                                     stp, 5, sizeof(BeamDebugTab))
                           : (const BeamDebugTab *)beamasm_get_rodata(
                                     stp->ba, "debug");
    if (debug_tab_ro == NULL) {
        return NULL;
    }
    debug_tab_rw_base = get_writable_ptr(stp->executable_region,
                                         stp->writable_region,
                                         debug_tab_ro);
    debug_tab_top = (BeamDebugTab *)debug_tab_rw_base;
    debug_tab_terms = (Eterm *)&debug_tab_top[1];
    debug_tab_items = (BeamDebugItem *)&debug_tab_terms[term_count];

    debug_tab_top->item_count = debug->item_count;
    debug_tab_top->items = debug_tab_items;

    for (i = 0; i < term_count; i++) {
        if (debug->is_literal[i]) {
            ASSERT(debug->is_literal[i] == 1);
            debug_tab_terms[i] =
                    beamfile_get_literal(&stp->beam, debug->terms[i]);
        } else {
            ASSERT(debug->is_literal[i] == 0);
            debug_tab_terms[i] = debug->terms[i];
        }
    }

    for (i = 0; i < item_count; i++) {
        Uint num_vars = debug->items[i].num_vars;
        Uint num_calls_terms = debug->items[i].num_calls_terms;

        debug_tab_items[i].location_index = debug->items[i].location_index;
        debug_tab_items[i].frame_size = debug->items[i].frame_size;
        debug_tab_items[i].num_vars = num_vars;
        debug_tab_items[i].num_calls_terms = num_calls_terms;
        debug_tab_items[i].first = debug_tab_terms;
        debug_tab_terms += 2 * num_vars + num_calls_terms;
    }

    return debug_tab_ro;
}

static int beam_load_finish_cache(LoaderState *stp) {
    BeamCodeHeader *header = (BeamCodeHeader *)stp->writable_region;
    Eterm *ptr;
    ErlOffHeap code_off_heap;
    ErtsLiteralArea *literal_area;
    Uint total_literals, allocation_size;
    const byte *data;
    Sint decoded_size;
    Uint i;

    header->coverage = stp->coverage;
    header->line_coverage_valid = stp->line_coverage_valid;
    header->loc_index_to_cover_id = stp->loc_index_to_cover_id;
    stp->coverage = NULL;
    stp->line_coverage_valid = NULL;
    stp->loc_index_to_cover_id = NULL;

    total_literals = stp->beam.static_literals.heap_size +
                     stp->beam.dynamic_literals.heap_size;
    ERTS_INIT_OFF_HEAP(&code_off_heap);
    allocation_size = ERTS_LITERAL_AREA_ALLOC_SIZE(total_literals);
    literal_area = erts_alloc(ERTS_ALC_T_LITERAL, allocation_size);
    ptr = &literal_area->start[0];
    literal_area->end = ptr + total_literals;
    beamfile_move_literals(&stp->beam, &ptr, &code_off_heap);
    for (i = 0; i < stp->jit_cache_fixup_count; i++) {
        BeamJitCachedFixup *fixup = &stp->jit_cache_fixups[i];
        if (fixup->kind == CACHE_FIXUP_LITERAL &&
            !cache_patch_word(stp,
                              fixup->offset,
                              fixup->addend,
                              beamfile_get_literal(&stp->beam,
                                                   (Sint32)fixup->index))) {
            return 0;
        }
    }
    literal_area->off_heap = code_off_heap.first;
    header->literal_area = literal_area;
    stp->load_hdr->literal_area = literal_area;

    /* Metadata-only preparation does not decode the instruction stream, so
     * line and debug indexes cannot be rebuilt yet. They are optional for
     * execution; leave them absent until the cache format carries them. */
    header->line_table = NULL;
    header->debug = NULL;

    if (stp->beam.attributes.size) {
        data = cache_named_rodata(stp, 1, stp->beam.attributes.size);
        if (data == NULL) return 0;
        header->attr_ptr = data;
        header->attr_size = stp->beam.attributes.size;
        decoded_size = erts_decode_ext_size(data, header->attr_size);
        if (decoded_size < 0) return 0;
        header->attr_size_on_heap = decoded_size;
    }
    if (stp->beam.compile_info.size) {
        data = cache_named_rodata(stp, 2, stp->beam.compile_info.size);
        if (data == NULL) return 0;
        header->compile_ptr = data;
        header->compile_size = stp->beam.compile_info.size;
        decoded_size = erts_decode_ext_size(data, header->compile_size);
        if (decoded_size < 0) return 0;
        header->compile_size_on_heap = decoded_size;
    }
    header->md5_ptr = cache_named_rodata(stp, 3, sizeof(stp->beam.checksum));
    if (header->md5_ptr == NULL) return 0;

    if (stp->beam.strings.size) {
        data = cache_named_rodata(stp, 0, stp->beam.strings.size);
        if (data == NULL) return 0;
        for (i = 0; i < stp->jit_cache_fixup_count; i++) {
            BeamJitCachedFixup *fixup = &stp->jit_cache_fixups[i];
            if (fixup->kind == CACHE_FIXUP_STRING &&
                !cache_patch_word(stp,
                                  fixup->offset,
                                  0,
                                  (UWord)(data + fixup->addend))) {
                return 0;
            }
        }
    }
    return 1;
}

int beam_load_finish_emit(LoaderState *stp) {
    const BeamCodeHeader *code_hdr_ro = NULL;
    BeamCodeHeader *code_hdr_rw = NULL;
    size_t module_size;
    char *module_base;
    Sint decoded_size;
    int ret;

    ret = 0;

    if (stp->line_instr != 0) {
        Uint line_size = offsetof(BeamCodeLineTab, func_tab);

        /* func_tab */
        line_size +=
                (stp->beam.code.function_count + 1) * sizeof(ErtsCodePtr *);

        /* line items */
        line_size += (stp->current_li + 1) * sizeof(ErtsCodePtr);

        /* fname table */
        line_size += stp->beam.lines.name_count * sizeof(Eterm);

        /* loc_tab */;
        line_size += (stp->current_li + 1) * stp->beam.lines.location_size;

        beamasm_embed_bss(stp->ba, "line", line_size);
    }

    /* Calculate size of the load BEAM debug information. */
    if (stp->beam.debug.item_count > 0) {
        BeamFile_DebugTable *debug = &stp->beam.debug;
        Uint debug_size;

        debug_size = sizeof(BeamDebugTab);
        debug_size += (Uint)debug->item_count * sizeof(BeamFile_DebugItem);
        debug_size += (Uint)debug->term_count * sizeof(Eterm);
        beamasm_embed_bss(stp->ba, "debug", debug_size);
    }

    /* Place the string table and, optionally, attributes here. */
    beamasm_embed_rodata(stp->ba,
                         "str",
                         (const char *)stp->beam.strings.data,
                         stp->beam.strings.size);
    beamasm_embed_rodata(stp->ba,
                         "attr",
                         (const char *)stp->beam.attributes.data,
                         stp->beam.attributes.size);
    beamasm_embed_rodata(stp->ba,
                         "compile",
                         (const char *)stp->beam.compile_info.data,
                         stp->beam.compile_info.size);
    beamasm_embed_rodata(stp->ba,
                         "md5",
                         (const char *)stp->beam.checksum,
                         sizeof(stp->beam.checksum));

    /* Transfer ownership of the coverage tables to the prepared code. */
    stp->load_hdr->coverage = stp->coverage;
    stp->load_hdr->line_coverage_valid = stp->line_coverage_valid;
    stp->load_hdr->loc_index_to_cover_id = stp->loc_index_to_cover_id;
    stp->coverage = NULL;
    stp->line_coverage_valid = NULL;
    stp->loc_index_to_cover_id = NULL;

    /* Move the code to its final location. */
    beamasm_codegen(stp->ba,
                    &stp->executable_region,
                    &stp->writable_region,
                    stp->load_hdr,
                    &code_hdr_ro,
                    &code_hdr_rw);

    stp->on_load = beamasm_get_on_load(stp->ba);
    module_base = beamasm_get_base(stp->ba);
    module_size = beamasm_get_offset(stp->ba);

    /* Save the updated code pointer and code size. */
    stp->code_hdr = code_hdr_ro;
    stp->loaded_size = module_size;

    /*
     * Place the literals in their own allocated heap (for fast range check)
     * and fix up all instructions that refer to it.
     */
    {
        Eterm *ptr;
        ErlOffHeap code_off_heap;
        ErtsLiteralArea *literal_area;
        Uint tot_lit_size;
        Uint lit_asize;
        int i;

        tot_lit_size = stp->beam.static_literals.heap_size +
                       stp->beam.dynamic_literals.heap_size;

        ERTS_INIT_OFF_HEAP(&code_off_heap);

        lit_asize = ERTS_LITERAL_AREA_ALLOC_SIZE(tot_lit_size);
        literal_area = erts_alloc(ERTS_ALC_T_LITERAL, lit_asize);
        ptr = &literal_area->start[0];
        literal_area->end = ptr + tot_lit_size;

        beamfile_move_literals(&stp->beam, &ptr, &code_off_heap);

        /* MAJOR HACK: literal identifiers are opaque at the moment. Make it
         * more like string patching. */
        for (i = 0; i < stp->beam.static_literals.count; i++) {
            Eterm lit = beamfile_get_literal(&stp->beam, i);
            beamasm_patch_literal(stp->ba, stp->writable_region, i, lit);
        }

        for (i = 0; i < stp->beam.dynamic_literals.count; i++) {
            Eterm lit = beamfile_get_literal(&stp->beam, ~i);
            beamasm_patch_literal(stp->ba, stp->writable_region, ~i, lit);
        }

        literal_area->off_heap = code_off_heap.first;
        code_hdr_rw->literal_area = literal_area;

        /* Ensure deallocation of literals in case the prepared code is
         * deallocated (without calling erlang:finish_loading/1). */
        (stp->load_hdr)->literal_area = literal_area;
    }

    /* Line information must be added after moving literals, since source file
     * names are literal lists. */
    code_hdr_rw->line_table = finish_line_table(stp, module_base, module_size);

    /* Debug information must be added after moving literals, since literals
     * are used extensively. */
    code_hdr_rw->debug = finish_debug_table(stp, module_base, module_size);

    if (stp->beam.attributes.size) {
        const byte *attr = beamasm_get_rodata(stp->ba, "attr");

        code_hdr_rw->attr_ptr = attr;
        code_hdr_rw->attr_size = stp->beam.attributes.size;

        decoded_size = erts_decode_ext_size(attr, code_hdr_rw->attr_size);
        if (decoded_size < 0) {
            BeamLoadError0(stp,
                           "bad external term representation of module "
                           "attributes");
        }

        code_hdr_rw->attr_size_on_heap = decoded_size;
    }

    if (stp->beam.compile_info.size) {
        const byte *compile_info = beamasm_get_rodata(stp->ba, "compile");

        code_hdr_rw->compile_ptr = compile_info;
        code_hdr_rw->compile_size = stp->beam.compile_info.size;

        decoded_size =
                erts_decode_ext_size(compile_info, stp->beam.compile_info.size);
        if (decoded_size < 0) {
            BeamLoadError0(stp,
                           "bad external term representation of compilation "
                           "information");
        }

        code_hdr_rw->compile_size_on_heap = decoded_size;
    }

    {
        const byte *md5_sum = beamasm_get_rodata(stp->ba, "md5");
        code_hdr_rw->md5_ptr = md5_sum;
    }

    /* Patch all instructions that refer to the string table. */
    if (stp->beam.strings.size) {
        const byte *string_table = beamasm_get_rodata(stp->ba, "str");
        beamasm_patch_strings(stp->ba, stp->writable_region, string_table);
    }

    /* Build private metadata before taking code loading permission.
     * Publication stays in finalization, after the last code patches. */
    beamasm_prepare_metadata(stp->ba, stp->code_hdr);

    ret = 1;
load_error:

    /* Some platforms use per-thread global permissions where a thread can
     * either write to or execute _ALL_ JITed pages, so we must seal the module
     * before yielding or this thread won't be able to execute any other JITed
     * code.
     *
     * Note that we have to do this regardless of whether we've succeeded or
     * not, as the module is unsealed after code generation. */
    beamasm_seal_module(stp->executable_region,
                        stp->writable_region,
                        stp->loaded_size);

    return ret;
}

void beam_load_finalize_code(LoaderState *stp,
                             struct erl_module_instance *inst_p) {
    ErtsCodeIndex staging_ix;
    int code_size;

    ERTS_LC_ASSERT(erts_initialized == 0 || erts_has_code_load_permission() ||
                   erts_thr_progress_is_blocking());

    code_size = stp->jit_cache_loaded
                        ? stp->loaded_size
                        : beamasm_get_header(stp->ba, &stp->code_hdr);
    erts_total_code_size += code_size;

    inst_p->executable_region = stp->executable_region;
    inst_p->writable_region = stp->writable_region;
    inst_p->code_hdr = stp->code_hdr;
    inst_p->code_length = code_size;
    inst_p->unsealed = 0;

    erts_unseal_module(inst_p);

#ifdef ADDRESS_SANITIZER
    /*
     * LeakSanitizer ignores directly mmap'ed memory by default. This causes
     * false positive leak reports of literal areas if only pointed to by
     * mmap'ed jit code. To suprress this we tell leak sanitizer that
     * the jit code is part of the "root set" when doing leak analysis.
     * Module purge should do a corresponding "unregister".
     */
    __lsan_register_root_region(inst_p->code_hdr, inst_p->code_length);
#endif

    /* Update ranges (used for finding a function from a PC value). */
    erts_update_ranges(inst_p->code_hdr, code_size);

    /* Allocate catch indices and fix up all catch_yf instructions. */
    if (stp->jit_cache_loaded) {
        unsigned catch_no = BEAM_CATCHES_NIL;
        for (int i = 0; i < stp->jit_cache_fixup_count; i++) {
            BeamJitCachedFixup *fixup = &stp->jit_cache_fixups[i];
            if (fixup->kind == CACHE_FIXUP_CATCH) {
                ErtsCodePtr handler;
                Eterm catch_term;
                ASSERT(fixup->target < stp->loaded_size);
                handler = (ErtsCodePtr)((const byte *)stp->executable_region +
                                        fixup->target);
                catch_no = beam_catches_cons(handler, catch_no, NULL);
                catch_term = make_catch(catch_no);
                if (!cache_patch_word(stp,
                                      fixup->offset,
                                      fixup->addend,
                                      catch_term)) {
                    erts_exit(ERTS_ABORT_EXIT, "invalid cached catch fixup\n");
                }
            }
        }
        inst_p->catches = catch_no;
    } else {
        inst_p->catches = beamasm_patch_catches(stp->ba,
                                                stp->writable_region);
    }

    /* Exported functions */
    staging_ix = erts_staging_code_ix();

    for (int i = 0; i < stp->beam.exports.count; i++) {
        BeamFile_ExportEntry *entry = &stp->beam.exports.entries[i];
        ErtsCodePtr address;
        Export *ep;

        if (stp->jit_cache_loaded) {
            ASSERT(entry->label < stp->jit_cache_label_count);
            address = (ErtsCodePtr)((const byte *)stp->executable_region +
                                    stp->jit_cache_labels[entry->label]);
        } else {
            address = beamasm_get_code(stp->ba, entry->label);
        }
        ep = erts_export_put(stp->module, entry->function, entry->arity);

        if (stp->on_load) {
            /* on_load: Don't make any of the exported functions
             * callable yet. Keep any function in the current
             * code callable. */
            ep->trampoline.not_loaded.deferred = (BeamInstr)address;
        } else {
            ep->dispatch.addresses[staging_ix] = address;
        }
    }

    /* Patch external function calls, this is done after exporting functions as
     * the module may remote-call itself*/
    for (int i = 0; i < stp->beam.imports.count; i++) {
        BeamFile_ImportEntry *entry = &stp->beam.imports.entries[i];
        const Export *import;

        import = erts_export_put(entry->module, entry->function, entry->arity);
        if (stp->jit_cache_loaded) {
            if (!cache_patch_fixups(stp,
                                    CACHE_FIXUP_IMPORT,
                                    i,
                                    (UWord)import)) {
                erts_exit(ERTS_ABORT_EXIT, "invalid cached import fixup\n");
            }
        } else {
            beamasm_patch_import(stp->ba, stp->writable_region, i, import);
        }
    }

    /* Patch fun creation. */
    if (stp->beam.lambdas.count) {
        BeamFile_LambdaTable *lambda_table = &stp->beam.lambdas;

        for (int i = 0; i < lambda_table->count; i++) {
            BeamFile_LambdaEntry *lambda;
            ErlFunEntry *fun_entry;

            lambda = &lambda_table->entries[i];

            fun_entry = erts_fun_entry_put(stp->module,
                                           lambda->old_uniq,
                                           i,
                                           stp->beam.checksum,
                                           lambda->index,
                                           lambda->arity - lambda->num_free);

            /* If there are no free variables, the loader has created a literal
             * for this lambda and we need to set its fun entry. */
            if (lambda->num_free == 0) {
                ErlFunThing *funp;
                Eterm fun;

                ASSERT(stp->lambda_literals[i] != ERTS_SWORD_MAX);
                fun = beamfile_get_literal(&stp->beam, stp->lambda_literals[i]);
                funp = (ErlFunThing *)boxed_val(fun);
                ASSERT(funp->entry.fun == NULL);
                funp->entry.fun = fun_entry;
            }

            if (stp->jit_cache_loaded) {
                ASSERT(stp->jit_cache_lambda_trampolines != NULL);
                ASSERT(stp->jit_cache_lambda_trampolines[i] != UINT64_MAX);
                erts_set_fun_code(
                        fun_entry,
                        staging_ix,
                        (ErtsCodePtr)((const byte *)stp->executable_region +
                                      stp->jit_cache_lambda_trampolines[i]));
                if (!cache_patch_fixups(stp,
                                        CACHE_FIXUP_LAMBDA,
                                        i,
                                        (UWord)fun_entry)) {
                    erts_exit(ERTS_ABORT_EXIT,
                              "invalid cached lambda fixup\n");
                }
            } else {
                erts_set_fun_code(fun_entry,
                                  staging_ix,
                                  beamasm_get_lambda(stp->ba, i));
                beamasm_patch_lambda(stp->ba,
                                     stp->writable_region,
                                     i,
                                     fun_entry);
            }
        }
    }

    {
        BeamFile_RecordTable rec = stp->beam.record;
        ErtsRecordEntry *entry;

        for (int i = 0; i < rec.record_count; i++) {
            Eterm def = beamfile_get_literal(&stp->beam,
                                             rec.records[i].def_literal);
            entry = erts_record_put(stp->module, rec.records[i].name);

            entry->definitions[staging_ix] = def;
        }

        erts_free(ERTS_ALC_T_PREPARED_CODE, rec.records);
        stp->beam.record.records = NULL;
    }

    /* Register debug / profiling info with external tools. */
    inst_p->metadata = stp->jit_cache_loaded
                               ? NULL
                               : beamasm_register_metadata(stp->ba,
                                                           stp->code_hdr);

    erts_seal_module(inst_p);

    /* Prevent literals and code from being freed. */
    (stp->load_hdr)->literal_area = NULL;
    stp->load_hdr->are_nifs = NULL;
    stp->load_hdr->coverage = NULL;
    stp->load_hdr->line_coverage_valid = NULL;
    stp->load_hdr->loc_index_to_cover_id = NULL;
    stp->executable_region = NULL;
    stp->writable_region = NULL;
    stp->code_hdr = NULL;
}

void beam_load_purge_aux(const BeamCodeHeader *hdr) {
    if (hdr->coverage) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->coverage);
    }

    if (hdr->line_coverage_valid) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->line_coverage_valid);
    }

    if (hdr->loc_index_to_cover_id) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->loc_index_to_cover_id);
    }
}
