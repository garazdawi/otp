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
 * BEAM file reader + placeholder compile_module.
 *
 * The reader does file I/O + SHA-256. The compile_module is a
 * placeholder that produces a CompiledModule containing the raw
 * BEAM bytes as "code" — this lets the end-to-end pipeline (read
 * → compile → write → close) be exercised before the real loader
 * is linked in. Replace cache_tool_compile_module() with the
 * real implementation once the loader builds.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#include "cache_tool.h"

/* ----------------------------------------------------------------
 * SHA-256, minimal public-domain implementation. Used for the
 * cache key. Replace with the runtime's crypto if/when linking
 * deeper. ~50 lines, no allocation, no error paths.
 * ---------------------------------------------------------------- */

#define ROR(x,n) (((x) >> (n)) | ((x) << (32-(n))))

static const uint32_t K[64] = {
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,
    0x923f82a4,0xab1c5ed5,0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,
    0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,0xe49b69c1,0xefbe4786,
    0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,
    0x06ca6351,0x14292967,0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,
    0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,0xa2bfe8a1,0xa81a664b,
    0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,
    0x5b9cca4f,0x682e6ff3,0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,
    0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
};

static void sha256_block(uint32_t h[8], const uint8_t buf[64]) {
    uint32_t w[64], a,b,c,d,e,f,g,hh,t1,t2;
    for (int i = 0; i < 16; i++) {
        w[i] = (uint32_t)buf[i*4]   << 24 | (uint32_t)buf[i*4+1] << 16
             | (uint32_t)buf[i*4+2] <<  8 | (uint32_t)buf[i*4+3];
    }
    for (int i = 16; i < 64; i++) {
        uint32_t s0 = ROR(w[i-15],7) ^ ROR(w[i-15],18) ^ (w[i-15] >> 3);
        uint32_t s1 = ROR(w[i-2],17) ^ ROR(w[i-2],19)  ^ (w[i-2]  >> 10);
        w[i] = w[i-16] + s0 + w[i-7] + s1;
    }
    a=h[0]; b=h[1]; c=h[2]; d=h[3]; e=h[4]; f=h[5]; g=h[6]; hh=h[7];
    for (int i = 0; i < 64; i++) {
        uint32_t S1 = ROR(e,6) ^ ROR(e,11) ^ ROR(e,25);
        uint32_t ch = (e & f) ^ (~e & g);
        t1 = hh + S1 + ch + K[i] + w[i];
        uint32_t S0 = ROR(a,2) ^ ROR(a,13) ^ ROR(a,22);
        uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
        t2 = S0 + maj;
        hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
    }
    h[0]+=a; h[1]+=b; h[2]+=c; h[3]+=d; h[4]+=e; h[5]+=f; h[6]+=g; h[7]+=hh;
}

static void sha256(const uint8_t *data, size_t len, uint8_t out[32]) {
    uint32_t h[8] = {0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,
                     0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19};
    size_t i = 0;
    while (len - i >= 64) { sha256_block(h, data + i); i += 64; }

    uint8_t buf[128] = {0};
    size_t r = len - i;
    memcpy(buf, data + i, r);
    buf[r] = 0x80;
    if (r >= 56) {
        sha256_block(h, buf);
        memset(buf, 0, 64);
    }
    uint64_t bitlen = (uint64_t)len * 8;
    for (int j = 0; j < 8; j++) buf[56+j] = (bitlen >> (56-8*j)) & 0xff;
    sha256_block(h, buf);

    for (int j = 0; j < 8; j++) {
        out[j*4]   = (h[j] >> 24) & 0xff;
        out[j*4+1] = (h[j] >> 16) & 0xff;
        out[j*4+2] = (h[j] >> 8)  & 0xff;
        out[j*4+3] =  h[j]        & 0xff;
    }
}

/* ----------------------------------------------------------------
 * BEAM file reader.
 * ---------------------------------------------------------------- */

int cache_tool_read_beam(const char *path, BeamInput *out) {
    memset(out, 0, sizeof(*out));
    out->path = path;

    int fd = open(path, O_RDONLY);
    if (fd < 0) return -1;

    struct stat st;
    if (fstat(fd, &st) < 0) { close(fd); return -2; }
    out->size = (size_t)st.st_size;

    out->data = malloc(out->size);
    if (!out->data) { close(fd); return -3; }

    ssize_t r = read(fd, out->data, out->size);
    close(fd);
    if (r != (ssize_t)out->size) { free(out->data); return -4; }

    sha256(out->data, out->size, out->sha256);

    /* Quick sanity check: BEAM files start with "FOR1" (IFF). */
    if (out->size < 12 || memcmp(out->data, "FOR1", 4) != 0) {
        free(out->data);
        return -5;
    }

    /* TODO: parse atom/code/etc chunks into beamfile_state for the
     * real compile_module path. For the placeholder below we only
     * need the raw bytes + name. */
    return 0;
}

void cache_tool_free_input(BeamInput *in) {
    if (!in) return;
    free(in->data);
    in->data = NULL;
    in->size = 0;
}

/* ----------------------------------------------------------------
 * Real compile_module — invokes the loader pipeline.
 *
 * Calls erts_prepare_loading which runs the BEAM parser +
 * transform engine + asmjit emit. Then extracts the assembled
 * code blob + reloc list (TODO) + literal pool from the LoaderState.
 *
 * The dyld runtime errors drive what stubs still need real
 * implementations: each first-call failure tells us exactly
 * which runtime hook the loader's happy path reached. We
 * implement them one at a time.
 *
 * Until the relocation infrastructure is wired in (emit_mov_*
 * recording calls in all instr_*.cpp sites), the code blob has
 * BIF pointers and atom values baked in for THIS tool's
 * process — useful for proving the pipeline works end-to-end
 * but not yet portable across VMs.
 * ---------------------------------------------------------------- */

/* Forward decls — these live in the runtime sources we linked in. */
typedef struct binary { char data[1]; } Binary;
typedef uintptr_t Eterm;
extern Binary *erts_alloc_loader_state(void);
extern Eterm erts_prepare_loading(Binary *magic, void *c_p,
                                  Eterm group_leader, Eterm *modp,
                                  const uint8_t *code, size_t size);

/* LoaderState lives behind a magic-binary wrapper in the runtime.
 * The cache_tool's stubbed erts_create_magic_binary (in stubs.c)
 * returns a malloc'd Binary*; we just need to fish the data out. */
extern void *ERTS_MAGIC_BIN_DATA_stub(Binary *b);  /* implemented in stubs */

int cache_tool_compile_module(const BeamInput *in, CompiledModule *out) {
    memset(out, 0, sizeof(*out));

    /* Module name: strip path + .beam suffix. */
    const char *slash = strrchr(in->path, '/');
    const char *base  = slash ? slash + 1 : in->path;
    size_t baselen = strlen(base);
    if (baselen > 5 && strcmp(base + baselen - 5, ".beam") == 0) baselen -= 5;
    char *name = malloc(baselen + 1);
    memcpy(name, base, baselen);
    name[baselen] = 0;
    out->module_name = name;

    memcpy(out->beam_sha256, in->sha256, 32);

    /* Drive the real loader. erts_alloc_loader_state creates the
     * LoaderState; erts_prepare_loading runs decode + transform +
     * specific selection + asmjit emit; on success, the LoaderState
     * has the assembled code ready to extract. */
    Binary *magic = erts_alloc_loader_state();
    if (!magic) {
        fprintf(stderr, "cache_tool: erts_alloc_loader_state returned NULL\n");
        return -1;
    }

    /* Intern the module name as an atom for the loader to compare
     * against the .beam's internal name. Use the real atom table
     * (linked from atom.c) so the value matches what the loader's
     * own erts_atom_put on the .beam's atom chunk will return. */
    extern Eterm erts_atom_put(const unsigned char *name, long len,
                               int enc, int trunc);
    Eterm module_atom = erts_atom_put((const unsigned char *)name,
                                      (long)strlen(name),
                                      2 /* ERTS_ATOM_ENC_UTF8 */, 0);

    Eterm result = erts_prepare_loading(magic, NULL /*c_p*/,
                                        0 /*NIL group_leader*/,
                                        &module_atom,
                                        in->data, in->size);

    /* NIL is the canonical "ok" return; anything else is an error
     * atom. NIL = 0x3B per BEAM Eterm tag encoding. */
    extern const char *cache_tool_atom_name(uint64_t);
    fprintf(stderr,
            "cache_tool: erts_prepare_loading(%s) returned %#lx (%s)\n",
            in->path, (unsigned long)result,
            cache_tool_atom_name(result));
    if (result != 0x3B /*NIL*/) {
        return -2;
    }

    out->loader_magic = magic;

    /* Pull the assembled native code AND the symbolic-relocation list
     * out of the LoaderState. After erts_prepare_loading returns NIL,
     * stp->executable_region points at the JIT'd code blob, and the
     * BeamModuleAssembler holds the relocs accumulated during emit
     * (one per converted call site). */
    extern void cache_tool_extract_from_loader(Binary *magic,
                                               const void **code_ptr,
                                               unsigned *code_size,
                                               const void **beam_file_ptr,
                                               const BeamJitRelocList **relocs_ptr);
    const void *code_blob = NULL;
    unsigned code_blob_size = 0;
    const BeamJitRelocList *relocs = NULL;
    cache_tool_extract_from_loader(magic, &code_blob, &code_blob_size,
                                   NULL, &relocs);
    if (!code_blob || code_blob_size == 0) {
        fprintf(stderr,
                "cache_tool: LoaderState has no executable region "
                "(loaded_size=%u, region=%p)\n",
                code_blob_size, code_blob);
        return -3;
    }
    out->code = malloc(code_blob_size);
    memcpy(out->code, code_blob, code_blob_size);
    out->code_size = code_blob_size;

    /* Populate the per-function table. */
    extern unsigned cache_tool_function_count(void *magic);
    extern int cache_tool_function_at(void *magic, unsigned i,
                                      unsigned *code_offset,
                                      unsigned *arity,
                                      char *name_buf, size_t buf_sz);
    unsigned fc = cache_tool_function_count(magic);
    if (fc > 0) {
        out->func_count = fc;
        out->funcs = calloc(fc, sizeof(*out->funcs));
        char nbuf[256];
        for (unsigned i = 0; i < fc; i++) {
            unsigned off = 0, ar = 0;
            cache_tool_function_at(magic, i, &off, &ar, nbuf, sizeof(nbuf));
            out->funcs[i].code_offset = off;
            out->funcs[i].arity = ar;
            /* name_str_idx 0 — the writer interns each function name
             * separately and would assign the real index. For our
             * round-trip we don't read these back yet. */
            out->funcs[i].name_str_idx = 0;
        }
    }

    /* Copy the reloc list into out->relocs and translate each entry's
     * symbolic_ref from a (BIF-table-local) value to an index in a
     * per-module string table.
     *
     * ATOM relocs hold the low 32 bits of the atom Eterm. Translate
     *   to a string name via the runtime's atom table.
     * BIF / EXPORT relocs hold an import-table index. Translate to a
     *   "Mod:Func/Arity" string via the BeamFile we extracted. */
    if (relocs && relocs->count > 0) {
        const void *beam_file = NULL;
        cache_tool_extract_from_loader(magic, NULL, NULL, &beam_file, NULL);

        out->reloc_count = relocs->count;
        out->relocs = malloc(relocs->count * sizeof(BeamJitReloc));
        memcpy(out->relocs, relocs->entries,
               relocs->count * sizeof(BeamJitReloc));

        /* Lazy per-module string tables — interned on first occurrence,
         * the reloc's symbolic_ref is rewritten to the table index. */
        size_t atom_cap = 64, mfa_cap = 64;
        out->atom_strings = malloc(atom_cap * sizeof(const char *));
        out->mfa_strings  = malloc(mfa_cap  * sizeof(const char *));

        extern size_t cache_tool_atom_name_bytes(uint32_t, const char **);
        extern size_t cache_tool_import_mfa_string(const void *,
                                                   uint32_t,
                                                   char *, size_t);

        for (size_t i = 0; i < out->reloc_count; i++) {
            BeamJitReloc *r = &out->relocs[i];
            char buf[512];
            const char *src = NULL;
            size_t src_len = 0;
            const char ***tbl  = NULL;
            size_t      *count = NULL;
            size_t      *cap   = NULL;

            switch (r->kind) {
            case BEAM_JIT_RELOC_ATOM: {
                const char *bytes;
                size_t nbytes = cache_tool_atom_name_bytes(r->symbolic_ref,
                                                           &bytes);
                if (nbytes && nbytes < sizeof(buf)) {
                    memcpy(buf, bytes, nbytes);
                    buf[nbytes] = 0;
                    src = buf;
                    src_len = nbytes;
                }
                tbl = &out->atom_strings;
                count = &out->atom_count;
                cap = &atom_cap;
                break;
            }
            case BEAM_JIT_RELOC_BIF:
            case BEAM_JIT_RELOC_EXPORT:
                if ((r->symbolic_ref & 0xffff0000u) == 0xffff0000u) {
                    /* emit_send sentinel — known BIF index, no MFA from
                     * import table. Encode as "<bif:N>" so the cache
                     * loader can resolve via the runtime bif_table. */
                    snprintf(buf, sizeof(buf),
                             "<bif:%u>", r->symbolic_ref & 0xffff);
                    src = buf;
                    src_len = strlen(buf);
                } else if (cache_tool_import_mfa_string(beam_file,
                                                       r->symbolic_ref,
                                                       buf, sizeof(buf))) {
                    src = buf;
                    src_len = strlen(buf);
                }
                tbl = &out->mfa_strings;
                count = &out->mfa_count;
                cap = &mfa_cap;
                break;
            case BEAM_JIT_RELOC_FRAGMENT_BRANCH: {
                /* symbolic_ref carries 0xfffe0000 | side_table_idx.
                 * The side table holds static labelName strings from
                 * BeamGlobalAssembler. Encode as "<frag:NAME>" in the
                 * mfa_strings table so the loader recognises the
                 * pattern and dispatches to fragment_addr_for_name. */
                extern const char *cache_tool_fragment_name_at(
                    void *magic, uint32_t idx);
                uint32_t idx = r->symbolic_ref & 0xffff;
                const char *fname = cache_tool_fragment_name_at(magic, idx);
                if (fname) {
                    snprintf(buf, sizeof(buf), "<frag:%s>", fname);
                    src = buf;
                    src_len = strlen(buf);
                }
                tbl = &out->mfa_strings;
                count = &out->mfa_count;
                cap = &mfa_cap;
                break;
            }
            case BEAM_JIT_RELOC_LITERAL:
                /* Pass through: symbolic_ref already carries the
                 * literal's per-module index. The cache writer
                 * serialises the literal blob separately; the loader
                 * uses the index to look up the live Eterm. */
                continue;
            case BEAM_JIT_RELOC_INTRA_LABEL: {
                /* Translate to byte offset within code. The high bit
                 * tags the encoding:
                 *   0xxx...: BeamLabel number (loader-assigned)
                 *   1xxx...: asmjit Label id (used for embed_label
                 *            sites without a BeamLabel handle). */
                extern unsigned beamasm_label_offset(void *instance,
                                                    unsigned label);
                extern unsigned beamasm_label_offset_by_asmjit_id(
                    void *instance, unsigned id);
                extern void *beamasm_get_assembler(void *magic);
                void *ba = beamasm_get_assembler(magic);
                if (r->symbolic_ref & 0x80000000u) {
                    r->symbolic_ref = beamasm_label_offset_by_asmjit_id(
                        ba, r->symbolic_ref & 0x7fffffffu);
                } else {
                    r->symbolic_ref = beamasm_label_offset(ba,
                        r->symbolic_ref);
                }
                continue;
            }
            default:
                continue;
            }

            if (!src) {
                r->symbolic_ref = 0xffffffffu; /* mark unresolved */
                continue;
            }

            /* Intern in the per-module string table. Dedup is linear —
             * O(n²) but n stays in the hundreds for preloaded modules. */
            uint32_t found = 0xffffffffu;
            for (size_t j = 0; j < *count; j++) {
                if (strcmp((*tbl)[j], src) == 0) {
                    found = (uint32_t)j;
                    break;
                }
            }
            if (found == 0xffffffffu) {
                if (*count == *cap) {
                    *cap *= 2;
                    *tbl = realloc(*tbl, *cap * sizeof(const char *));
                }
                (*tbl)[*count] = strdup(src);
                found = (uint32_t)(*count)++;
                (void)src_len;
            }
            r->symbolic_ref = found;
        }
    }

    return 0;
}

void cache_tool_free_compiled(CompiledModule *cm) {
    if (!cm) return;
    free((void *)cm->module_name);
    free(cm->code);
    free(cm->relocs);
    for (size_t i = 0; i < cm->atom_count; i++) free((void *)cm->atom_strings[i]);
    free((void *)cm->atom_strings);
    for (size_t i = 0; i < cm->mfa_count; i++)  free((void *)cm->mfa_strings[i]);
    free((void *)cm->mfa_strings);
    free(cm->literal_blob);
    free(cm->literal_relocs);
    free(cm->funcs);
    memset(cm, 0, sizeof(*cm));
}
