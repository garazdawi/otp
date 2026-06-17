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
 * Fallback table for the runtime_fn host hook: symbols dlsym can't
 * see (file-scope statics, function-local statics inside methods)
 * register their (demangled-name → address) here so the validator
 * can resolve RUNTIME_FN relocs against them. The runtime emulator
 * needs the same mechanism — a separate runtime table will mirror
 * this when we wire the load path.
 *
 * Registration happens at the affected sites via the static-init
 * trick:
 *     static bool reg = (cache_tool_register_static("name", &x), true);
 *
 * Lookup is linear (under 64 entries expected).
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define CACHE_TOOL_STATIC_TABLE_MAX 1024

static struct {
    const char *name;
    void *addr;
} g_table[CACHE_TOOL_STATIC_TABLE_MAX];
static int g_count = 0;

void cache_tool_register_static(const char *name, void *addr) {
    /* Idempotent: re-registering the same (name, addr) is a no-op.
     * Useful because function-local statics may have their static-
     * init trick re-evaluated across translation units (e.g. when
     * the cache_tool builds both a debug and release variant in
     * the same process — not currently, but cheap to guard). */
    for (int i = 0; i < g_count; i++) {
        if (g_table[i].addr == addr && strcmp(g_table[i].name, name) == 0) {
            return;
        }
    }
    if (g_count >= CACHE_TOOL_STATIC_TABLE_MAX) {
        fprintf(stderr,
                "cache_tool_static_table: full, can't register %s\n", name);
        return;
    }
    g_table[g_count].name = name;
    g_table[g_count].addr = addr;
    g_count++;
}

void *cache_tool_static_symbol_addr(const char *name) {
    for (int i = 0; i < g_count; i++) {
        if (strcmp(g_table[i].name, name) == 0) {
            return g_table[i].addr;
        }
    }
    return NULL;
}
