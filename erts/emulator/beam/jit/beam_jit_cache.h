/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

#ifndef BEAM_JIT_CACHE_H
#define BEAM_JIT_CACHE_H

#include <stddef.h>
#include <stdint.h>

typedef struct {
    const unsigned char *data;
    size_t size;
    uint32_t section_count;
    uint32_t relocation_count;
    uint64_t image_size;
    const unsigned char *sections;
    const unsigned char *relocations;
    const unsigned char *image;
} BeamJitCache;

typedef struct {
    uint32_t type;
    uint32_t target_section;
    uint64_t payload;
} BeamJitCacheRelocation;

/* Return non-zero and store the address to accept a relocation target. */
typedef int (*BeamJitCacheResolver)(void *context,
                                    const BeamJitCacheRelocation *relocation,
                                    uintptr_t default_target,
                                    uintptr_t *target);

/* Parse and fully validate an EJITC001 AArch64 image. */
int beam_jit_cache_open(const unsigned char *data,
                        size_t size,
                        BeamJitCache *cache);

/* Copy and relocate an image into an existing dual-mapped JIT allocation. */
int beam_jit_cache_load_aarch64(const BeamJitCache *cache,
                                void *writable_base,
                                const void *executable_base,
                                size_t allocation_size,
                                BeamJitCacheResolver resolver,
                                void *resolver_context);

#endif
