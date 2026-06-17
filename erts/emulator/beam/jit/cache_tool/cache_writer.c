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
 * Cache file writer for the beam_jit_compile tool.
 *
 * Writes the format documented in cache_tool/README.md and
 * beam_jit_cache.h. The runtime cache loader in beam_load.c walks
 * the same structure in reverse.
 *
 * NOT FUNCTIONAL — this file shows the intended layout. The actual
 * implementation has to handle endianness, alignment, atomic write
 * (tempfile + rename), and the two output modes (sidecar / bundle).
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "cache_tool.h"

struct CacheWriter {
    FILE   *fp;
    char   *tmp_path;     /* atomic write: write to tmp, rename on close */
    char   *final_path;
    CacheHeader header;
    long    toc_offset;   /* for bundle mode */
    size_t  module_count;
    /* String table accumulated across modules in bundle mode;
     * deduplicates atoms and MFAs across the bundle. Cache-load
     * time builds one atom_value[] for the whole bundle, not per
     * module — smaller cache + fewer interns. */
    struct {
        char  **strings;
        size_t  count;
        size_t  capacity;
    } strtab;
};

/* ----------------------------------------------------------------
 * Header layout (matches the design in IDEAS/07 §1a):
 *
 *   offset  size   field
 *   0       4      magic           "EJC1"
 *   4       8      erts_vsn        major.minor.patch packed
 *   12      32     jit_vsn         git hash of emitter
 *   44      4      arch            CacheArch enum
 *   48      16     cflags_hash
 *   64      4      module_count
 *   68      4      string_off
 *   72      4      lit_off
 *   76      4      code_off
 *   80      ...    per-module entries
 * ---------------------------------------------------------------- */

#define MAGIC "EJC1"

CacheWriter *cache_writer_open(const char *path, const CacheHeader *hdr) {
    CacheWriter *w = calloc(1, sizeof(*w));
    if (!w) return NULL;

    /* Atomic-write pattern: write to <path>.tmp, fsync, rename. */
    size_t n = strlen(path) + 5;
    w->tmp_path   = malloc(n);
    w->final_path = strdup(path);
    snprintf(w->tmp_path, n, "%s.tmp", path);

    w->fp = fopen(w->tmp_path, "wb");
    if (!w->fp) {
        free(w->tmp_path); free(w->final_path); free(w);
        return NULL;
    }
    w->header = *hdr;

    /* Write a placeholder header — patched at close() with the
     * final TOC offset / module count. */
    char placeholder[80] = {0};
    memcpy(placeholder, MAGIC, 4);
    fwrite(placeholder, sizeof(placeholder), 1, w->fp);
    return w;
}

/* ----------------------------------------------------------------
 * String table dedup. Module emit calls this; on bundle close the
 * strings are flushed in their interned order.
 * ---------------------------------------------------------------- */

static uint32_t strtab_intern(CacheWriter *w, const char *s) {
    for (size_t i = 0; i < w->strtab.count; i++) {
        if (strcmp(w->strtab.strings[i], s) == 0) return (uint32_t)i;
    }
    if (w->strtab.count == w->strtab.capacity) {
        size_t cap = w->strtab.capacity * 2;
        if (cap < 64) cap = 64;
        w->strtab.strings = realloc(w->strtab.strings,
                                    cap * sizeof(char *));
        w->strtab.capacity = cap;
    }
    w->strtab.strings[w->strtab.count] = strdup(s);
    return (uint32_t)w->strtab.count++;
}

/* ----------------------------------------------------------------
 * Per-module emit. Each module entry on disk is:
 *
 *   - module name str_idx (4 bytes)
 *   - beam_sha256 (32 bytes)
 *   - code_size (4) + code_blob (code_size bytes)
 *   - reloc_count (4) + relocs (reloc_count * sizeof(BeamJitReloc))
 *   - atom_count (4) + atom_str_indices (atom_count * 4)
 *   - mfa_count (4) + mfa_str_indices (mfa_count * 4)
 *   - literal_size (4) + literal_blob (literal_size bytes)
 *   - literal_reloc_count (4) + literal_relocs
 *   - func_count (4) + funcs (func_count * 12 bytes each)
 *   - has_on_load (1) + padding
 *
 * All multi-byte fields little-endian; relocs use the same encoding
 * as the in-memory BeamJitReloc struct (POD).
 * ---------------------------------------------------------------- */

int cache_writer_emit_module(CacheWriter *w, const CompiledModule *cm) {
    uint32_t name_idx = strtab_intern(w, cm->module_name);

    /* Module name */
    fwrite(&name_idx,   4, 1, w->fp);
    fwrite(cm->beam_sha256, 32, 1, w->fp);

    /* Code. Canonicalise before write so the .jc is byte-reproducible:
     *   - zero the BeamCodeHeader region (loader rebuilds it),
     *   - zero every slot a reloc will overwrite (loader patches it).
     * After this, any leftover difference between two .jc's compiled
     * from the same .beam is a real non-determinism bug — and a stale
     * byte that wasn't overwritten at load time will crash hard
     * instead of executing on someone else's old pointer. */
    extern unsigned cache_tool_code_header_size(void *magic);
    uint32_t code_size = (uint32_t)cm->code_size;
    fwrite(&code_size,  4, 1, w->fp);
    uint8_t *canon = malloc(cm->code_size);
    if (!canon) return -1;
    memcpy(canon, cm->code, cm->code_size);
    size_t hdr = cache_tool_code_header_size(cm->loader_magic);
    hdr = (hdr + 7) & ~(size_t)7;
    if (hdr > cm->code_size) hdr = cm->code_size;
    memset(canon, 0, hdr);
    for (size_t i = 0; i < cm->reloc_count; i++) {
        const BeamJitReloc *r = &cm->relocs[i];
        if ((size_t)r->code_offset + r->imm_width > cm->code_size) continue;
        memset(canon + r->code_offset, 0, r->imm_width);
    }
    fwrite(canon, cm->code_size, 1, w->fp);
    free(canon);

    /* Relocs */
    uint32_t rc = (uint32_t)cm->reloc_count;
    fwrite(&rc,         4, 1, w->fp);
    fwrite(cm->relocs,  sizeof(BeamJitReloc), cm->reloc_count, w->fp);

    /* Atom string indices for this module (into bundle strtab) */
    uint32_t ac = (uint32_t)cm->atom_count;
    fwrite(&ac, 4, 1, w->fp);
    for (size_t i = 0; i < cm->atom_count; i++) {
        uint32_t idx = strtab_intern(w, cm->atom_strings[i]);
        fwrite(&idx, 4, 1, w->fp);
    }

    /* MFA strings */
    uint32_t mc = (uint32_t)cm->mfa_count;
    fwrite(&mc, 4, 1, w->fp);
    for (size_t i = 0; i < cm->mfa_count; i++) {
        uint32_t idx = strtab_intern(w, cm->mfa_strings[i]);
        fwrite(&idx, 4, 1, w->fp);
    }

    /* Literal pool */
    uint32_t lit_size = (uint32_t)cm->literal_size;
    fwrite(&lit_size, 4, 1, w->fp);
    if (cm->literal_size) {
        fwrite(cm->literal_blob, cm->literal_size, 1, w->fp);
    }
    uint32_t lrc = (uint32_t)cm->literal_reloc_count;
    fwrite(&lrc, 4, 1, w->fp);
    fwrite(cm->literal_relocs, sizeof(BeamJitReloc),
           cm->literal_reloc_count, w->fp);

    /* Func table */
    uint32_t fc = (uint32_t)cm->func_count;
    fwrite(&fc, 4, 1, w->fp);
    for (size_t i = 0; i < cm->func_count; i++) {
        fwrite(&cm->funcs[i].name_str_idx, 4, 1, w->fp);
        fwrite(&cm->funcs[i].arity,        4, 1, w->fp);
        fwrite(&cm->funcs[i].code_offset,  4, 1, w->fp);
    }

    uint8_t has_on_load = cm->has_on_load ? 1 : 0;
    fwrite(&has_on_load, 1, 1, w->fp);
    /* Pad to 4-byte boundary for the next module entry. */
    static const uint8_t zero3[3] = {0,0,0};
    fwrite(zero3, 3, 1, w->fp);

    w->module_count++;
    return 0;
}

/* ----------------------------------------------------------------
 * Close: flush the string table, patch the header with the final
 * offsets / module count, fsync, atomic rename.
 * ---------------------------------------------------------------- */

void cache_writer_close(CacheWriter *w) {
    if (!w) return;
    if (w->fp) {
        /* String table is appended at the end and the header is
         * patched with its offset. */
        long strtab_off = ftell(w->fp);
        uint32_t strtab_count = (uint32_t)w->strtab.count;
        fwrite(&strtab_count, 4, 1, w->fp);
        for (size_t i = 0; i < w->strtab.count; i++) {
            const char *s = w->strtab.strings[i];
            uint32_t len = (uint32_t)strlen(s);
            fwrite(&len, 4, 1, w->fp);
            fwrite(s, len, 1, w->fp);
        }

        /* Patch header. */
        fseek(w->fp, 64, SEEK_SET);
        uint32_t mc = (uint32_t)w->module_count;
        fwrite(&mc, 4, 1, w->fp);
        uint32_t off = (uint32_t)strtab_off;
        fwrite(&off, 4, 1, w->fp);

        fflush(w->fp);
        fsync(fileno(w->fp));
        fclose(w->fp);

        /* Atomic rename. */
        rename(w->tmp_path, w->final_path);
    }

    for (size_t i = 0; i < w->strtab.count; i++) free(w->strtab.strings[i]);
    free(w->strtab.strings);
    free(w->tmp_path);
    free(w->final_path);
    free(w);
}
