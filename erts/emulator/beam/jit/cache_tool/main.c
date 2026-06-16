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
 * beam_jit_compile — entry point sketch.
 *
 * Build-time tool for IDEAS/07 #1. Reads one or more `.beam` files,
 * runs them through the same loader + asmjit emit pipeline the live
 * emulator uses (with runtime hooks replaced by recording stubs),
 * and writes either:
 *
 *   - one .jc file per input (sidecar style), or
 *   - one bundled .jitcache file with a TOC + per-module entries
 *     (the boot-cache style).
 *
 * See README.md in this directory for the full design.
 *
 * NOT FUNCTIONAL — this file shows the intended structure and CLI;
 * the actual implementations live in cache_tool_stubs.c and
 * cache_writer.c, and call into the unmodified loader + asmjit
 * emitter source.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#include "cache_tool.h"

static void usage(const char *prog, int rc) {
    fprintf(stderr,
            "usage: %s [options] <input.beam> [...]\n"
            "\n"
            "  --arch <a>        target arch: x86_64 | aarch64\n"
            "                    (default: host arch)\n"
            "  --out <path>      output file path (required)\n"
            "  --bundle          bundle multiple inputs into one .jitcache\n"
            "                    (otherwise emits one .jc per input alongside --out)\n"
            "  --erts-vsn <v>    target ERTS version string\n"
            "  --jit-vsn <hash>  JIT emitter git hash (cache key)\n"
            "  --cflags-hash <h> hash of relevant +J* flags (cache key)\n"
            "  --verbose         print per-module timings and stats\n"
            "  --help            this message\n",
            prog);
    exit(rc);
}

int main(int argc, char **argv) {
    struct {
        const char *arch;
        const char *out;
        const char *erts_vsn;
        const char *jit_vsn;
        const char *cflags_hash;
        int bundle;
        int verbose;
    } opts = {
        .arch = HOST_ARCH_STRING,
        .out  = NULL,
        .erts_vsn = ERTS_VSN_DEFAULT,
        .jit_vsn  = JIT_VSN_DEFAULT,
        .cflags_hash = CFLAGS_HASH_DEFAULT,
        .bundle = 0,
        .verbose = 0,
    };

    static struct option longopts[] = {
        {"arch",        required_argument, 0, 'a'},
        {"out",         required_argument, 0, 'o'},
        {"bundle",      no_argument,       0, 'b'},
        {"erts-vsn",    required_argument, 0, 'e'},
        {"jit-vsn",     required_argument, 0, 'j'},
        {"cflags-hash", required_argument, 0, 'f'},
        {"verbose",     no_argument,       0, 'v'},
        {"help",        no_argument,       0, 'h'},
        {0, 0, 0, 0}
    };

    int c;
    while ((c = getopt_long(argc, argv, "a:o:be:j:f:vh", longopts, NULL)) != -1) {
        switch (c) {
        case 'a': opts.arch = optarg; break;
        case 'o': opts.out = optarg; break;
        case 'b': opts.bundle = 1; break;
        case 'e': opts.erts_vsn = optarg; break;
        case 'j': opts.jit_vsn = optarg; break;
        case 'f': opts.cflags_hash = optarg; break;
        case 'v': opts.verbose = 1; break;
        case 'h': usage(argv[0], 0);
        default:  usage(argv[0], 1);
        }
    }

    if (!opts.out || optind >= argc) usage(argv[0], 1);

    /* Initialise the loader + asmjit + stub layer just enough that
     * the cache_tool_compile_module() path below can run. The stub
     * layer's init sets up: malloc-backed allocators, sequential
     * atom indexer, empty BIF/export tables that record symbolic
     * references. */
    if (cache_tool_init(opts.arch) != 0) {
        fprintf(stderr, "init failed for arch %s\n", opts.arch);
        return 2;
    }

    /* Open the writer. In bundle mode this writes a single output
     * with a TOC + per-module entries; in sidecar mode each input
     * gets its own file derived from opts.out. */
    CacheWriter *w = cache_writer_open(opts.out, &(CacheHeader){
        .erts_vsn    = opts.erts_vsn,
        .jit_vsn     = opts.jit_vsn,
        .cflags_hash = opts.cflags_hash,
        .arch        = arch_from_string(opts.arch),
        .bundle      = opts.bundle,
    });
    if (!w) {
        fprintf(stderr, "cannot open output %s\n", opts.out);
        return 3;
    }

    int rc = 0;
    for (int i = optind; i < argc; i++) {
        const char *beam_path = argv[i];

        /* Read the .beam file. Same path as the runtime's
         * beamfile_read(), via the stub'd erts_alloc. */
        BeamInput in;
        if (cache_tool_read_beam(beam_path, &in) != 0) {
            fprintf(stderr, "read failed: %s\n", beam_path);
            rc = 4;
            continue;
        }

        /* Run the loader: BEAM decode + ops.tab transforms +
         * specific-op selection + per-op asmjit emit. The emit
         * calls go to the unmodified jit/<arch>/instr_*.cpp,
         * which in turn use the emit_mov_* wrappers (from
         * beam_jit_cache_emit.hpp) that ALSO record symbolic
         * references on a per-module reloc list. */
        CompiledModule cm;
        if (cache_tool_compile_module(&in, &cm) != 0) {
            fprintf(stderr, "compile failed: %s\n", beam_path);
            cache_tool_free_input(&in);
            rc = 5;
            continue;
        }

        if (opts.verbose) {
            fprintf(stderr,
                    "  %-40s %6zu bytes code, %4zu relocs, %4zu atoms\n",
                    beam_path, cm.code_size, cm.reloc_count, cm.atom_count);
        }

        /* Serialise: code blob + reloc lists + literal pool +
         * symbol tables. The reader counterpart in beam_load.c
         * walks the same structure in reverse. */
        if (cache_writer_emit_module(w, &cm) != 0) {
            fprintf(stderr, "write failed: %s\n", beam_path);
            rc = 6;
        }

        cache_tool_free_compiled(&cm);
        cache_tool_free_input(&in);
    }

    cache_writer_close(w);
    cache_tool_shutdown();
    return rc;
}
