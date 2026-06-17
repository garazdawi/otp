/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

/*
 * Runtime side of IDEAS/07 #1 — loads a cache file produced by the
 * standalone beam_jit_compile tool. Mmap the file, parse the header,
 * find the requested module's entry, copy its code blob into a fresh
 * mmap, walk the reloc list and patch each entry to point at the live
 * VM's address for that symbol.
 *
 * The parser is self-contained (no asmjit, no loader globals) so it
 * can be used by the cache_tool itself to round-trip-test what it
 * just wrote.
 *
 * Address resolution is delegated to a small set of host hooks:
 *   - atom_eterm_for_name(name, n)        → live atom Eterm
 *   - export_ptr_for_mfa(mfa_str)         → live Export*
 *   - bif_fn_for_index(N)                 → live BIF function ptr
 *   - vm_static_for_id(which)             → addr of named runtime static
 *
 * The host (runtime emulator OR cache_tool) supplies them. With those
 * in hand, the loader is purely mechanical bytes-in / bytes-out.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "beam_jit_cache.h"
#include "beam_jit_cache_load.h"

/* ---- mmap helpers ---------------------------------------------- */

static int file_mmap(const char *path, const uint8_t **data, size_t *size) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) return -1;
    struct stat st;
    if (fstat(fd, &st) < 0) { close(fd); return -2; }
    void *m = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (m == MAP_FAILED) return -3;
    *data = (const uint8_t *)m;
    *size = st.st_size;
    return 0;
}

static void file_unmap(const void *data, size_t size) {
    if (data) munmap((void *)data, size);
}

/* ---- header layout (matches cache_writer.c) -------------------- */

#define CACHE_MAGIC "EJC1"
#define CACHE_HDR_SIZE 80
#define CACHE_MODCOUNT_OFF 64
#define CACHE_STRTAB_OFF_OFF 68

/* ---- little-endian byte readers -------------------------------- */

static uint32_t read_u32(const uint8_t *p) {
    return (uint32_t)p[0]
         | ((uint32_t)p[1] << 8)
         | ((uint32_t)p[2] << 16)
         | ((uint32_t)p[3] << 24);
}

/* ---- cache open / close ---------------------------------------- */

struct BeamJitCache_ {
    const uint8_t *data;
    size_t         size;

    uint32_t       module_count;
    uint32_t       strtab_off;

    /* Lazy-parsed strtab: indices → c-string pointers into the mmap. */
    const char   **strtab;
    uint32_t       strtab_count;
};

static int parse_strtab(BeamJitCache *c) {
    if (c->strtab_off + 4 > c->size) return -1;
    const uint8_t *p = c->data + c->strtab_off;
    c->strtab_count = read_u32(p); p += 4;
    c->strtab = calloc(c->strtab_count, sizeof(const char *));
    if (!c->strtab) return -2;
    for (uint32_t i = 0; i < c->strtab_count; i++) {
        if ((size_t)(p - c->data) + 4 > c->size) return -3;
        uint32_t len = read_u32(p); p += 4;
        if ((size_t)(p - c->data) + len > c->size) return -4;
        /* Strings in the file aren't NUL-terminated, but the writer
         * writes them contiguously. For our consumers we copy into a
         * fresh NUL-terminated buffer. */
        char *s = malloc(len + 1);
        if (!s) return -5;
        memcpy(s, p, len);
        s[len] = 0;
        c->strtab[i] = s;
        p += len;
    }
    return 0;
}

BeamJitCache *beam_jit_cache_open(const char *path) {
    BeamJitCache *c = calloc(1, sizeof(*c));
    if (!c) return NULL;
    if (file_mmap(path, &c->data, &c->size) != 0) { free(c); return NULL; }
    if (c->size < CACHE_HDR_SIZE
        || memcmp(c->data, CACHE_MAGIC, 4) != 0) {
        file_unmap(c->data, c->size); free(c);
        return NULL;
    }
    c->module_count = read_u32(c->data + CACHE_MODCOUNT_OFF);
    c->strtab_off   = read_u32(c->data + CACHE_STRTAB_OFF_OFF);
    if (parse_strtab(c) != 0) {
        beam_jit_cache_close(c);
        return NULL;
    }
    return c;
}

void beam_jit_cache_close(BeamJitCache *c) {
    if (!c) return;
    if (c->strtab) {
        for (uint32_t i = 0; i < c->strtab_count; i++) {
            free((void *)c->strtab[i]);
        }
        free(c->strtab);
    }
    file_unmap(c->data, c->size);
    free(c);
}

/* ---- per-module parsing ---------------------------------------- */

/* Walk the per-module entries until we find one whose name matches.
 * Module entries are laid out exactly as documented in cache_writer.c.
 *
 * Returns 0 + populates `out` on hit, <0 on miss / corruption. */
static int find_module(const BeamJitCache *c, const char *name,
                       BeamJitCacheModule *out) {
    /* Module entries start right after the 80-byte header. */
    const uint8_t *p = c->data + CACHE_HDR_SIZE;
    const uint8_t *end = c->data + c->strtab_off;

    for (uint32_t m = 0; m < c->module_count && p < end; m++) {
        if (p + 4 + 32 + 4 > end) return -1;
        uint32_t name_idx = read_u32(p); p += 4;
        memcpy(out->beam_sha256, p, 32); p += 32;
        uint32_t code_size = read_u32(p); p += 4;
        const uint8_t *code_p = p; p += code_size;

        if (p + 4 > end) return -2;
        uint32_t reloc_count = read_u32(p); p += 4;
        const uint8_t *relocs_p = p;
        p += reloc_count * sizeof(BeamJitReloc);

        if (p + 4 > end) return -3;
        uint32_t atom_count = read_u32(p); p += 4;
        const uint8_t *atoms_p = p;
        p += atom_count * 4;

        if (p + 4 > end) return -4;
        uint32_t mfa_count = read_u32(p); p += 4;
        const uint8_t *mfas_p = p;
        p += mfa_count * 4;

        /* Literal pool: ignored for now, just skip past. */
        if (p + 4 > end) return -5;
        uint32_t lit_size = read_u32(p); p += 4 + lit_size;
        if (p + 4 > end) return -6;
        uint32_t lrc = read_u32(p); p += 4 + lrc * (uint32_t)sizeof(BeamJitReloc);

        /* Func table — skip past as well; we read code only here. */
        if (p + 4 > end) return -7;
        uint32_t fc = read_u32(p); p += 4 + fc * 12;

        /* has_on_load + 3-byte pad */
        if (p + 4 > end) return -8;
        uint8_t has_on_load = *p; p += 4;

        /* Resolve the module name from the bundle strtab. */
        if (name_idx >= c->strtab_count) return -9;
        if (strcmp(c->strtab[name_idx], name) != 0) continue;

        out->name         = c->strtab[name_idx];
        out->code         = code_p;
        out->code_size    = code_size;
        out->relocs       = (const BeamJitReloc *)relocs_p;
        out->reloc_count  = reloc_count;
        out->atom_indices = atoms_p;
        out->atom_count   = atom_count;
        out->mfa_indices  = mfas_p;
        out->mfa_count    = mfa_count;
        out->has_on_load  = has_on_load;
        return 0;
    }
    return -100; /* not found */
}

/* ---- the loader -------------------------------------------------
 *
 * Public entry: given a cache, a module name, and a set of host hooks
 * that resolve symbolic refs to addresses, return the loaded+patched
 * code as a fresh allocation. Caller owns it.
 * ---------------------------------------------------------------- */

int beam_jit_cache_load_module(BeamJitCache *c,
                               const char *module_name,
                               const BeamJitCacheHostHooks *hooks,
                               void **out_code,
                               size_t *out_size) {
    BeamJitCacheModule m;
    int rc = find_module(c, module_name, &m);
    if (rc != 0) return rc;

    /* Allocate the loaded-code region close enough to the live global
     * fragments that an aarch64 BL can reach. We mmap with the address
     * of a known global fragment as a placement hint — the OS will pick
     * a nearby slot. malloc would put us in the heap, which on macOS is
     * gigabytes away from JIT-allocator regions and outside BL's
     * ±128 MB reach. The runtime emulator's real loader uses the JIT
     * allocator directly, which never has this problem. */
    void *fragment_hint = hooks->fragment_addr_for_name
        ? hooks->fragment_addr_for_name(hooks->ctx,
                                        "i_func_info_shared") : NULL;
    size_t page = 4096;
    size_t alloc_size = (m.code_size + page - 1) & ~(page - 1);
    uint8_t *code = NULL;
    if (fragment_hint) {
        /* Try just past the fragment region (within 8 MB) so we're
         * comfortably inside BL reach in either direction. */
        void *hint = (uint8_t *)fragment_hint + (8 << 20);
        code = mmap(hint, alloc_size, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANON, -1, 0);
        if (code == MAP_FAILED) code = NULL;
    }
    if (!code) {
        code = malloc(m.code_size);
        if (!code) return -10;
    }
    memcpy(code, m.code, m.code_size);

    size_t patched = 0, skipped_unresolved = 0, skipped_width = 0;
    /* Walk each reloc; resolve symbolic_ref via the host hooks. */
    for (size_t i = 0; i < m.reloc_count; i++) {
        const BeamJitReloc *r = &m.relocs[i];
        uintptr_t value = 0;
        int resolved = 0;

        switch (r->kind) {
        case BEAM_JIT_RELOC_ATOM: {
            if (r->symbolic_ref >= m.atom_count) break;
            uint32_t str_idx = read_u32(m.atom_indices + r->symbolic_ref * 4);
            if (str_idx >= c->strtab_count) break;
            const char *name = c->strtab[str_idx];
            value = (uintptr_t)hooks->atom_eterm_for_name(
                hooks->ctx, name, strlen(name));
            resolved = 1;
            break;
        }
        case BEAM_JIT_RELOC_BIF:
        case BEAM_JIT_RELOC_EXPORT: {
            if (r->symbolic_ref >= m.mfa_count) break;
            uint32_t str_idx = read_u32(m.mfa_indices + r->symbolic_ref * 4);
            if (str_idx >= c->strtab_count) break;
            const char *s = c->strtab[str_idx];
            if (s[0] == '<' && strncmp(s, "<bif:", 5) == 0) {
                uint32_t bif_idx = (uint32_t)atoi(s + 5);
                value = (uintptr_t)hooks->bif_fn_for_index(
                    hooks->ctx, bif_idx);
            } else if (r->kind == BEAM_JIT_RELOC_EXPORT) {
                value = (uintptr_t)hooks->export_ptr_for_mfa(hooks->ctx, s);
            } else {
                value = (uintptr_t)hooks->bif_fn_for_mfa(hooks->ctx, s);
            }
            resolved = 1;
            break;
        }
        case BEAM_JIT_RELOC_VM_STATIC: {
            value = (uintptr_t)hooks->vm_static_for_id(
                hooks->ctx, r->symbolic_ref);
            resolved = 1;
            break;
        }
        case BEAM_JIT_RELOC_LITERAL: {
            value = hooks->literal_eterm_for_index(
                hooks->ctx, r->symbolic_ref);
            resolved = (value != 0);
            break;
        }
        case BEAM_JIT_RELOC_INTRA_LABEL:
            /* symbolic_ref holds the byte offset within the code blob.
             * Patch the 8-byte slot with the absolute loaded address. */
            value = (uintptr_t)code + r->symbolic_ref;
            resolved = 1;
            break;
        case BEAM_JIT_RELOC_RUNTIME_FN: {
            /* The reader encoded the C symbol name into the per-module
             * mfa_strings table with prefix "<rfn:". Strip and resolve
             * via the runtime_fn host hook. */
            if (r->symbolic_ref >= m.mfa_count) break;
            uint32_t str_idx = read_u32(m.mfa_indices + r->symbolic_ref * 4);
            if (str_idx >= c->strtab_count) break;
            const char *s = c->strtab[str_idx];
            if (strncmp(s, "<rfn:", 5) != 0) break;
            char name[256];
            size_t nl = strlen(s);
            if (nl < 6 || nl - 5 >= sizeof(name)) break;
            memcpy(name, s + 5, nl - 6);
            name[nl - 6] = 0;
            if (!hooks->runtime_fn_for_symbol) break;
            value = (uintptr_t)hooks->runtime_fn_for_symbol(hooks->ctx, name);
            resolved = (value != 0);
            break;
        }
        case BEAM_JIT_RELOC_BYTE_PTR: {
            /* symbolic_ref is the offset into the BEAM file's StrT
             * chunk. The host hook returns the live address of that
             * byte (i.e. string_table_base + offset). */
            if (!hooks->byte_ptr_addr) break;
            value = (uintptr_t)hooks->byte_ptr_addr(hooks->ctx,
                                                   r->symbolic_ref);
            resolved = (value != 0);
            break;
        }
        case BEAM_JIT_RELOC_FRAGMENT_BRANCH: {
            if (r->symbolic_ref >= m.mfa_count) {
                fprintf(stderr, "  FRAG: symref %u >= mfa_count %zu\n",
                        r->symbolic_ref, m.mfa_count);
                break;
            }
            uint32_t str_idx = read_u32(m.mfa_indices + r->symbolic_ref * 4);
            if (str_idx >= c->strtab_count) break;
            const char *s = c->strtab[str_idx];
            if (strncmp(s, "<frag:", 6) != 0) {
                fprintf(stderr, "  FRAG: not a frag string '%s'\n", s);
                break;
            }
            char name[256];
            size_t nl = strlen(s);
            if (nl < 7 || nl - 6 >= sizeof(name)) break;
            memcpy(name, s + 6, nl - 7);
            name[nl - 7] = 0;
            value = (uintptr_t)hooks->fragment_addr_for_name(
                hooks->ctx, name);
            if (!value) {
                fprintf(stderr, "  FRAG: unresolved '%s'\n", name);
            }
            resolved = (value != 0);
            break;
        }
        default:
            break;
        }

        if (!resolved) { skipped_unresolved++; continue; }

        if (r->kind == BEAM_JIT_RELOC_FRAGMENT_BRANCH
            && r->imm_width == 4
            && r->code_offset + 4 <= m.code_size) {
            /* Rewrite the aarch64 BL instruction at code_offset to
             * target the live fragment address. Encoding:
             *   BL imm26 = 0x94000000 | (imm26 & 0x03ffffff)
             * imm26 is signed offset / 4 from this instruction's PC. */
            uintptr_t pc = (uintptr_t)(code + r->code_offset);
            intptr_t delta = (intptr_t)(value - pc);
            /* Sanity: BL's range is ±128 MB. If we can't reach, leave
             * the existing instruction in place; the loader would need
             * a veneer slot, which the cache writer hasn't reserved. */
            if (delta >= -(intptr_t)(1 << 27) && delta < (intptr_t)(1 << 27)
                && (delta & 3) == 0) {
                int32_t imm26 = (int32_t)(delta / 4);
                uint32_t insn = 0x94000000u
                    | ((uint32_t)imm26 & 0x03ffffffu);
                memcpy(code + r->code_offset, &insn, 4);
                patched++;
            } else {
                skipped_width++;
            }
            continue;
        }

        if (r->imm_width == 8 && r->code_offset + 8 <= m.code_size) {
            memcpy(code + r->code_offset, &value, 8);
            patched++;
        } else if (r->imm_width >= 4 && r->imm_width <= 16
                   && (r->imm_width & 3) == 0
                   && r->code_offset + r->imm_width <= m.code_size) {
            /* aarch64 MOVZ + MOVK*N sequence — N+1 4-byte instructions,
             * each carrying a 16-bit slice of the 64-bit immediate.
             * Decode the destination register from the first instr;
             * rewrite each instr with the new slice for its shift.
             *
             *   MOVZ  Xd, #imm16, LSL #shift   shift = 0
             *     0xD2800000 | (shift/16 << 21) | (imm16 << 5) | Rd
             *   MOVK  Xd, #imm16, LSL #shift   shift = 16, 32, 48
             *     0xF2800000 | (shift/16 << 21) | (imm16 << 5) | Rd
             *
             * MOVZ is also synthesised as ORR with ZR for the value 0
             * (a.mov-with-imm(0) path), which we don't patch here —
             * the recorded width covers only the MOVZ+MOVK form. */
            uint32_t *insns = (uint32_t *)(code + r->code_offset);
            unsigned n = r->imm_width / 4;
            uint32_t Rd = insns[0] & 0x1f;
            for (unsigned k = 0; k < n; k++) {
                uint16_t slice = (uint16_t)((value >> (16 * k)) & 0xffff);
                uint32_t opcode_top = (k == 0) ? 0xD2800000u  /* MOVZ */
                                               : 0xF2800000u; /* MOVK */
                uint32_t shift_field = (uint32_t)k << 21;
                insns[k] = opcode_top | shift_field
                         | ((uint32_t)slice << 5) | Rd;
            }
            patched++;
        } else {
            skipped_width++;
        }
    }

    fprintf(stderr, "  load %s: %zu relocs total, %zu patched, "
                    "%zu unresolved, %zu width-deferred\n",
            module_name, m.reloc_count, patched,
            skipped_unresolved, skipped_width);
    /* For debugging: dump first 30 reloc records sorted by offset. */
    if (1) {
        for (size_t ii = 0; ii < m.reloc_count && ii < 30; ii++) {
            fprintf(stderr, "    reloc[%zu] off=%u kind=%u w=%u ref=%u\n",
                    ii, m.relocs[ii].code_offset, m.relocs[ii].kind,
                    m.relocs[ii].imm_width, m.relocs[ii].symbolic_ref);
        }
    }

    *out_code = code;
    *out_size = m.code_size;
    return 0;
}
