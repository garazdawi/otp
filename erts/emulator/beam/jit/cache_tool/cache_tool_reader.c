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
 * Placeholder compile_module.
 *
 * Until the real loader is linked in, produce a fake CompiledModule
 * containing the BEAM bytes verbatim as "code". This exercises the
 * full read → CompiledModule → write pipeline; the cache file
 * produced is valid for the format but contains BEAM bytecode where
 * machine code should be — the runtime loader would reject it (or,
 * with a follow-up patch, fall through to load the BEAM via the
 * regular path).
 *
 * Real implementation: invoke the loader (beam_load.c) → asmjit
 * emitters; collect the resulting code blob + reloc list + literal
 * pool.
 * ---------------------------------------------------------------- */

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

    /* Placeholder: copy BEAM bytes into the "code" slot. The cache
     * format reader will see a code blob of the right shape but
     * without actual machine code; for the no-op MVP that's fine
     * — runtime falls through to the BEAM loader on cache-miss. */
    out->code = malloc(in->size);
    memcpy(out->code, in->data, in->size);
    out->code_size = in->size;

    /* No relocs, no atoms, no literals yet — the real compile
     * path populates these from the loader's output. */
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
