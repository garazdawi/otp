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
 * --validate-deterministic mode: cross-process regression guard.
 *
 * Same-process determinism (the existing --validate check) is too
 * permissive — it doesn't catch bytes that are stable within a
 * process but vary across processes (host-static addresses, ASLR-
 * sensitive pointers, etc.). The right property to assert is:
 *
 *   "Two writer processes invoked on the same .beam produce
 *    byte-identical .jc files."
 *
 * That property holds iff every process-specific byte in the code
 * blob is covered by a recorded reloc and zeroed at write time.
 * If a new mov-imm site sneaks in without being recorded, this
 * test fires.
 *
 * Implementation: invoke ourselves twice via system() to produce
 * two .jc files for the same .beam (each subprocess gets its own
 * ASLR slide), then walk both files module-by-module and report
 * any byte difference along with whether it's covered by a reloc.
 *
 * Pass iff no byte differs anywhere. Fail with a per-difference
 * diagnostic otherwise.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#include "cache_tool.h"
#include "../beam_jit_cache_load.h"

/* Self-invoke: produce a .jc file at out_path from beam_path using
 * the same executable. Returns 0 on success. */
static int spawn_compile(const char *self_exe,
                         const char *arch,
                         const char *beam_path,
                         const char *out_path) {
    char cmd[4096];
    int n = snprintf(cmd, sizeof(cmd),
                     "%s --arch %s --out %s %s >/dev/null 2>&1",
                     self_exe, arch, out_path, beam_path);
    if (n < 0 || (size_t)n >= sizeof(cmd)) return -1;
    int rc = system(cmd);
    if (rc != 0) {
        fprintf(stderr,
                "validate-det: child compile failed (rc=%d): %s\n",
                rc, cmd);
        return -2;
    }
    return 0;
}

/* Infer the module name from the .beam path: strip directory and
 * .beam extension. "erts/preloaded/ebin/prim_eval.beam" → "prim_eval". */
static char *infer_module_name_from_path(const char *beam_path) {
    const char *slash = strrchr(beam_path, '/');
    const char *base = slash ? slash + 1 : beam_path;
    const char *dot = strrchr(base, '.');
    size_t len = dot ? (size_t)(dot - base) : strlen(base);
    char *out = malloc(len + 1);
    if (!out) return NULL;
    memcpy(out, base, len);
    out[len] = 0;
    return out;
}

/* Compare two modules' code byte-for-byte. Print a per-difference
 * diagnostic. Returns 0 if identical, count of differing bytes
 * otherwise. */
static int compare_module_code(const BeamJitCacheModule *a,
                               const BeamJitCacheModule *b,
                               const char *mod_name,
                               int verbose) {
    if (a->code_size != b->code_size) {
        fprintf(stderr,
                "FAIL %s: code size differs A=%zu B=%zu\n",
                mod_name, a->code_size, b->code_size);
        return (int)(a->code_size > b->code_size
                     ? a->code_size : b->code_size);
    }
    int diffs = 0;
    int uncovered = 0;
    size_t first = (size_t)-1;
    for (size_t i = 0; i < a->code_size; i++) {
        if (a->code[i] == b->code[i]) continue;
        diffs++;
        if (first == (size_t)-1) first = i;
        int covered = 0;
        for (size_t r = 0; r < a->reloc_count; r++) {
            const BeamJitReloc *rl = &a->relocs[r];
            if (i >= rl->code_offset
                && i < rl->code_offset + rl->imm_width) {
                covered = 1;
                break;
            }
        }
        if (!covered) {
            uncovered++;
            if (verbose && uncovered <= 8) {
                fprintf(stderr,
                        "  uncovered diff at off=%zu  A=0x%02x B=0x%02x\n",
                        i, a->code[i], b->code[i]);
            }
        }
    }
    if (diffs == 0) {
        return 0;
    }
    fprintf(stderr,
            "FAIL %s: %d byte diffs, %d uncovered (first at off=%zu)\n",
            mod_name, diffs, uncovered, first);
    return diffs;
}

int cache_tool_validate_deterministic(const char *self_exe,
                                      const char *arch,
                                      const char *beam_path,
                                      int verbose) {
    char path_a[256], path_b[256];
    pid_t pid = getpid();
    snprintf(path_a, sizeof(path_a),
             "/tmp/cache_tool_det_a_%d.jc", (int)pid);
    snprintf(path_b, sizeof(path_b),
             "/tmp/cache_tool_det_b_%d.jc", (int)pid);

    if (spawn_compile(self_exe, arch, beam_path, path_a) != 0
        || spawn_compile(self_exe, arch, beam_path, path_b) != 0) {
        unlink(path_a);
        unlink(path_b);
        return 2;
    }

    BeamJitCache *ca = beam_jit_cache_open(path_a);
    BeamJitCache *cb = beam_jit_cache_open(path_b);
    int rc = 0;
    if (!ca || !cb) {
        fprintf(stderr, "validate-det: cannot reopen produced .jc files\n");
        rc = 3;
        goto done;
    }

    char *mod = infer_module_name_from_path(beam_path);
    if (!mod) {
        fprintf(stderr, "validate-det: cannot infer module name from %s\n",
                beam_path);
        rc = 4;
        goto done;
    }

    BeamJitCacheModule ma, mb;
    if (beam_jit_cache_find_module(ca, mod, &ma) != 0
        || beam_jit_cache_find_module(cb, mod, &mb) != 0) {
        fprintf(stderr,
                "validate-det: module %s not in both .jc files\n", mod);
        rc = 5;
        goto done_mod;
    }

    int diffs = compare_module_code(&ma, &mb, mod, verbose);
    if (diffs == 0) {
        fprintf(stderr, "PASS %s: %zu code bytes byte-identical "
                        "across two writer processes\n",
                mod, ma.code_size);
        rc = 0;
    } else {
        rc = 1;
    }

done_mod:
    free(mod);
done:
    if (ca) beam_jit_cache_close(ca);
    if (cb) beam_jit_cache_close(cb);
    unlink(path_a);
    unlink(path_b);
    return rc;
}
