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
 * Runtime-side fallback table for the cache loader's
 * runtime_fn_for_symbol hook. Mirrors cache_tool_static_table.c
 * but populated explicitly: the symbols here are referenced by C
 * name so the compiler gives us their addresses, even when their
 * linkage is internal (static, static inline) or when the runtime
 * build strips them from the dynamic symbol table.
 *
 * Each entry holds the *mangled* name as the cache writer recorded
 * it (via dladdr at compile time) plus the address taken in THIS
 * TU. For external-linkage symbols the address is the unique global
 * one; for static-inline helpers we get this TU's inline copy,
 * which is behaviorally identical to any other copy.
 *
 * Build-time invariant: every RUNTIME_FN reloc in a cached
 * preloaded module must resolve via either dlsym (for symbols in
 * the dynamic symbol table) or this table. The validator's
 * --validate-deterministic guard surfaces the set; new entries get
 * added when a new symbol shows up.
 */

extern "C" {
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_proc_sig_queue.h"
#include "beam_common.h"
#include "bif.h"
}
#include <string.h>

/* Forward declarations for the beam_jit_* helpers (their full
 * prototypes live in jit/beam_jit_common.hpp which is C++-only;
 * pull them in here as C decls, sufficient to take their
 * addresses). The BIF entries (bump_reductions_1, etc.) come from
 * erl_bif_table.h with the right signature already. */
Sint32 beam_jit_remove_message(Process *c_p, Sint32, Eterm *, Eterm *,
                               Uint32);
void beam_jit_wait_locked(Process *c_p, ErtsCodePtr cp);
void beam_jit_wait_timeout(Process *c_p, Eterm timeout, ErtsCodePtr cp);
void beam_jit_wait_unlocked(Process *c_p, ErtsCodePtr cp);
void beam_jit_take_receive_lock(Process *c_p);
void beam_jit_timeout(Process *c_p);
void beam_jit_timeout_locked(Process *c_p);
void beam_jit_i_emit_nyi(const char *msg);
/* JIT_HARD_DEBUG-only; only declared so we can stub-resolve the
 * cache file's RUNTIME_FN reloc against a known address even when
 * the real function isn't compiled in. Provide a fallback stub
 * below. */
#if defined(JIT_HARD_DEBUG)
void beam_jit_invalid_heap_ptr(Process *p, Eterm term);
#else
static void beam_jit_invalid_heap_ptr(Process *p, Eterm term) {
    (void)p; (void)term;
    erts_exit(ERTS_ABORT_EXIT,
              "beam_jit_invalid_heap_ptr called in non-debug build\n");
}
#endif

/* The fallback table. Each entry: (mangled-name, address). Add new
 * entries here when --validate-deterministic surfaces a new
 * unresolved RUNTIME_FN at boot. */
static const struct {
    const char *name;
    const void *addr;
} g_table[] = {
    /* Anchor erts_msgq_set_save_next so its static-inline expansion
     * exists in THIS TU. Address its inline copy. */
    {"_ZL23erts_msgq_set_save_nextP7process",
     (const void *)erts_msgq_set_save_next},

    {"_Z23beam_jit_remove_messageP7processiPmS1_j",
     (const void *)beam_jit_remove_message},
    {"_Z20beam_jit_wait_lockedP7processPKv",
     (const void *)beam_jit_wait_locked},
    {"_Z21beam_jit_wait_timeoutP7processmPKv",
     (const void *)beam_jit_wait_timeout},
    {"_Z22beam_jit_wait_unlockedP7processPKv",
     (const void *)beam_jit_wait_unlocked},
    {"_Z26beam_jit_take_receive_lockP7process",
     (const void *)beam_jit_take_receive_lock},
    {"_Z16beam_jit_timeoutP7process",
     (const void *)beam_jit_timeout},
    {"_Z23beam_jit_timeout_lockedP7process",
     (const void *)beam_jit_timeout_locked},
    {"_Z19beam_jit_i_emit_nyiPKc",
     (const void *)beam_jit_i_emit_nyi},
    {"_Z24beam_jit_invalid_heap_ptrPv",
     (const void *)beam_jit_invalid_heap_ptr},

    {"bump_reductions_1",  (const void *)bump_reductions_1},
    {"get_module_info_1",  (const void *)get_module_info_1},
    {"get_module_info_2",  (const void *)get_module_info_2},
};

extern "C" void *cache_runtime_static_symbol_addr(const char *name) {
    for (size_t i = 0; i < sizeof(g_table) / sizeof(g_table[0]); i++) {
        if (strcmp(g_table[i].name, name) == 0) {
            return (void *)g_table[i].addr;
        }
    }
    return NULL;
}
