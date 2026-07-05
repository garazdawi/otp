/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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

/*
 * T2-Full tier-2 JIT: retention copy/free/accounting. See t2_retain.h.
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_alloc.h"
#include "beam_file.h"
#include "module.h"

#include "t2_retain.h"

static erts_atomic_t t2_retained_bytes;

int erts_t2_enabled(void) {
    static int enabled = -1;

    if (enabled < 0) {
        const char *env = getenv("T2_RETAIN");
        enabled = (env != NULL && env[0] == '1') ? 1 : 0;
    }

    return enabled;
}

/* Diagnostics on stderr for every retention decision (T2_DEBUG=1). */
static int t2_debug_enabled(void) {
    static int enabled = -1;

    if (enabled < 0) {
        const char *env = getenv("T2_DEBUG");
        enabled = (env != NULL && env[0] == '1') ? 1 : 0;
    }

    return enabled;
}

void erts_t2_init(void) {
    erts_atomic_init_nob(&t2_retained_bytes, 0);
}

UWord erts_t2_retained_sz(void) {
    return (UWord)erts_atomic_read_nob(&t2_retained_bytes);
}

static size_t align_up(size_t size) {
    return (size + (sizeof(UWord) - 1)) & ~(sizeof(UWord) - 1);
}

void erts_t2_retain(BeamFile *beam, struct erl_module_instance *inst_p) {
    ErtsT2RetainedCode *ret;
    Uint32 *bitmap;
    int any_eligible = 0;

    size_t atoms_size, imports_size, types_size, literals_size, bitmap_size;
    size_t code_size, total;
    size_t offset;
    byte *base;
    Sint32 i;

    ASSERT(inst_p->t2_retained == NULL);

    bitmap = erts_t2_eligibility_scan(beam, &any_eligible);
    if (t2_debug_enabled()) {
        erts_fprintf(stderr,
                     "t2_retain: module=%T functions=%d any_eligible=%d\n",
                     beam->module,
                     beam->code.function_count,
                     any_eligible);
    }
    if (!any_eligible) {
        if (bitmap != NULL) {
            erts_free(ERTS_ALC_T_T2_CODE, bitmap);
        }
        return;
    }

    atoms_size = beam->atoms.count * sizeof(Eterm);
    imports_size = beam->imports.count * sizeof(BeamFile_ImportEntry);
    types_size = beam->types.count * sizeof(BeamType);
    literals_size = beam->static_literals.count * sizeof(Eterm);
    bitmap_size = ((beam->code.function_count + 31) / 32) * sizeof(Uint32);
    code_size = beam->code.size;

    /* One allocation: header, then the word-aligned tables, then the
     * unaligned code bytes last. */
    total = align_up(sizeof(ErtsT2RetainedCode)) + align_up(atoms_size) +
            align_up(imports_size) + align_up(types_size) +
            align_up(literals_size) + align_up(bitmap_size) + code_size;

    base = erts_alloc(ERTS_ALC_T_T2_CODE, total);
    ret = (ErtsT2RetainedCode *)base;
    sys_memset(ret, 0, sizeof(ErtsT2RetainedCode));

    offset = align_up(sizeof(ErtsT2RetainedCode));

    ret->atoms = (Eterm *)(base + offset);
    ret->atom_count = beam->atoms.count;
    sys_memcpy(ret->atoms, beam->atoms.entries, atoms_size);
    offset += align_up(atoms_size);

    ret->imports = (BeamFile_ImportEntry *)(base + offset);
    ret->import_count = beam->imports.count;
    sys_memcpy(ret->imports, beam->imports.entries, imports_size);
    offset += align_up(imports_size);

    ret->types = (BeamType *)(base + offset);
    ret->type_count = beam->types.count;
    ret->types_fallback = beam->types.fallback;
    sys_memcpy(ret->types, beam->types.entries, types_size);
    offset += align_up(types_size);

    /* beamfile_move_literals has already run, so entry values are
     * literal-area terms that stay valid until purge. */
    ret->literal_map = (Eterm *)(base + offset);
    ret->literal_count = beam->static_literals.count;
    for (i = 0; i < beam->static_literals.count; i++) {
        ASSERT(beam->static_literals.entries[i].heap_fragments == NULL);
        ret->literal_map[i] = beam->static_literals.entries[i].value;
    }
    offset += align_up(literals_size);

    ret->eligible_bitmap = (Uint32 *)(base + offset);
    sys_memcpy(ret->eligible_bitmap, bitmap, bitmap_size);
    offset += align_up(bitmap_size);

    ret->code = base + offset;
    ret->code_size = beam->code.size;
    ret->function_count = beam->code.function_count;
    ret->label_count = beam->code.label_count;
    ret->max_opcode = beam->code.max_opcode;
    sys_memcpy(ret->code, beam->code.data, code_size);
    offset += code_size;

    ASSERT(offset == total);
    ret->bytes = total;

    erts_free(ERTS_ALC_T_T2_CODE, bitmap);

    inst_p->t2_retained = ret;
    erts_atomic_add_nob(&t2_retained_bytes, (erts_aint_t)total);

    if (t2_debug_enabled()) {
        erts_fprintf(stderr,
                     "t2_retain: module=%T bytes=%lu total_retained=%lu\n",
                     beam->module,
                     (unsigned long)total,
                     (unsigned long)erts_t2_retained_sz());
    }
}

void erts_t2_release(struct erl_module_instance *inst_p) {
    ErtsT2RetainedCode *ret = inst_p->t2_retained;

    if (ret == NULL) {
        return;
    }

    erts_atomic_add_nob(&t2_retained_bytes, -(erts_aint_t)ret->bytes);
    erts_free(ERTS_ALC_T_T2_CODE, ret);

    inst_p->t2_retained = NULL;
}
