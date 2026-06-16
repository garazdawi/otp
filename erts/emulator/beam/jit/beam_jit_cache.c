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
 * Implementation of the BeamJitRelocList primitives declared in
 * beam_jit_cache.h. Used by both the runtime JIT (when persistent
 * cache writing is on) and the standalone cache_tool. No dependencies
 * on sys.h or the rest of the runtime — just stdlib.
 */

#include <stdlib.h>
#include <string.h>

#include "beam_jit_cache.h"

void beam_jit_reloc_list_init(BeamJitRelocList *list) {
    list->entries = NULL;
    list->count = 0;
    list->capacity = 0;
}

void beam_jit_reloc_list_free(BeamJitRelocList *list) {
    free(list->entries);
    list->entries = NULL;
    list->count = 0;
    list->capacity = 0;
}

void beam_jit_reloc_append(BeamJitRelocList *list,
                           uint32_t code_offset,
                           BeamJitRelocKind kind,
                           uint16_t imm_width,
                           uint32_t symbolic_ref) {
    if (list->count == list->capacity) {
        size_t new_cap = list->capacity ? list->capacity * 2 : 64;
        BeamJitReloc *new_entries =
            (BeamJitReloc *)realloc(list->entries,
                                    new_cap * sizeof(BeamJitReloc));
        if (!new_entries) return; /* OOM — silently drop the reloc */
        list->entries = new_entries;
        list->capacity = new_cap;
    }
    list->entries[list->count].code_offset  = code_offset;
    list->entries[list->count].kind         = (uint16_t)kind;
    list->entries[list->count].imm_width    = imm_width;
    list->entries[list->count].symbolic_ref = symbolic_ref;
    list->count++;
}
