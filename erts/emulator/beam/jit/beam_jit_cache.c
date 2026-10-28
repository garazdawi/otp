/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

#include "beam_jit_cache.h"

#include <limits.h>
#include <string.h>

#define CACHE_HEADER_SIZE 80
#define CACHE_SECTION_SIZE 40
#define CACHE_RELOCATION_SIZE 64
#define CACHE_ARCH_AARCH64 1

#define RELOC_REL_TO_ABS 4

#define OFFSET_SIGNED 0
#define OFFSET_UNSIGNED 1

static uint32_t get_u32(const unsigned char *p) {
    return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 |
           (uint32_t)p[3] << 24;
}

static uint64_t get_u64(const unsigned char *p) {
    uint64_t value = 0;
    unsigned i;

    for (i = 0; i < 8; i++) {
        value |= (uint64_t)p[i] << (i * 8);
    }
    return value;
}

static int add_u64(uint64_t a, uint64_t b, uint64_t *result) {
    if (b > UINT64_MAX - a) {
        return 0;
    }
    *result = a + b;
    return 1;
}

static int mul_u64(uint64_t a, uint64_t b, uint64_t *result) {
    if (a != 0 && b > UINT64_MAX / a) {
        return 0;
    }
    *result = a * b;
    return 1;
}

static const unsigned char *section_at(const BeamJitCache *cache,
                                       uint32_t index) {
    return cache->sections + (size_t)index * CACHE_SECTION_SIZE;
}

static const unsigned char *find_section(const BeamJitCache *cache,
                                         uint32_t id) {
    uint32_t i;

    for (i = 0; i < cache->section_count; i++) {
        const unsigned char *section = section_at(cache, i);
        if (get_u32(section) == id) {
            return section;
        }
    }
    return NULL;
}

static int ranges_overlap(uint64_t a, uint64_t a_size,
                          uint64_t b, uint64_t b_size) {
    return a_size != 0 && b_size != 0 && a < b + b_size && b < a + a_size;
}

static int validate_format(const unsigned char *relocation) {
    uint32_t type = get_u32(relocation);
    uint32_t format = get_u32(relocation + 4);
    uint32_t flags = get_u32(relocation + 8);
    uint32_t region_size = get_u32(relocation + 20);
    uint32_t value_offset = get_u32(relocation + 24);
    uint32_t value_size = get_u32(relocation + 28);
    uint32_t bit_count = get_u32(relocation + 32);
    uint32_t bit_shift = get_u32(relocation + 36);
    uint32_t discard_lsb = get_u32(relocation + 40);

    if (type != RELOC_REL_TO_ABS || flags != 0 || region_size == 0 ||
        (value_size != 1 && value_size != 2 && value_size != 4 &&
         value_size != 8) ||
        value_offset > region_size || value_size > region_size - value_offset ||
        bit_count == 0 || bit_count > value_size * 8 ||
        bit_shift >= value_size * 8 || bit_count > value_size * 8 - bit_shift ||
        discard_lsb > 63) {
        return 0;
    }

    return format == OFFSET_SIGNED || format == OFFSET_UNSIGNED;
}

int beam_jit_cache_open(const unsigned char *data,
                        size_t size,
                        BeamJitCache *cache) {
    uint32_t section_count, relocation_count;
    uint64_t section_bytes, relocation_bytes, relocations_offset;
    uint64_t image_offset, image_size, end;
    BeamJitCache parsed;
    uint32_t i, j;

    if (data == NULL || cache == NULL || size < CACHE_HEADER_SIZE ||
        memcmp(data, "EJITC001", 8) != 0 || get_u32(data + 8) != 1 ||
        get_u32(data + 12) != CACHE_HEADER_SIZE ||
        get_u32(data + 16) != CACHE_ARCH_AARCH64 || get_u32(data + 28) != 0) {
        return 0;
    }

    section_count = get_u32(data + 20);
    relocation_count = get_u32(data + 24);
    image_size = get_u64(data + 32);
    relocations_offset = get_u64(data + 64);
    image_offset = get_u64(data + 72);
    if (section_count == 0 ||
        get_u64(data + 56) != CACHE_HEADER_SIZE ||
        !mul_u64(section_count, CACHE_SECTION_SIZE, &section_bytes) ||
        !add_u64(CACHE_HEADER_SIZE, section_bytes, &end) ||
        relocations_offset != end ||
        !mul_u64(relocation_count, CACHE_RELOCATION_SIZE, &relocation_bytes) ||
        !add_u64(relocations_offset, relocation_bytes, &end) ||
        image_offset != end || !add_u64(image_offset, image_size, &end) ||
        end > size || image_size > SIZE_MAX) {
        return 0;
    }

    parsed.data = data;
    parsed.section_count = section_count;
    parsed.relocation_count = relocation_count;
    parsed.image_size = image_size;
    parsed.sections = data + CACHE_HEADER_SIZE;
    parsed.relocations = data + relocations_offset;
    parsed.image = data + image_offset;

    for (i = 0; i < section_count; i++) {
        const unsigned char *section = section_at(&parsed, i);
        uint32_t id = get_u32(section);
        uint32_t alignment = get_u32(section + 8);
        uint64_t offset = get_u64(section + 16);
        uint64_t buffer_size = get_u64(section + 24);
        uint64_t virtual_size = get_u64(section + 32);
        uint64_t mapped_size = virtual_size ? virtual_size : buffer_size;

        if (alignment == 0) alignment = 1;
        if (id >= section_count ||
            (alignment & (alignment - 1)) != 0 || offset % alignment != 0 ||
            buffer_size > mapped_size || offset > image_size ||
            mapped_size > image_size - offset) {
            return 0;
        }
        for (j = 0; j < i; j++) {
            const unsigned char *other = section_at(&parsed, j);
            if (get_u32(other) == id ||
                ranges_overlap(offset, mapped_size,
                               get_u64(other + 16),
                               get_u64(other + 32)
                                       ? get_u64(other + 32)
                                       : get_u64(other + 24))) {
                return 0;
            }
        }
    }

    for (i = 0; i < relocation_count; i++) {
        const unsigned char *relocation =
                parsed.relocations + (size_t)i * CACHE_RELOCATION_SIZE;
        const unsigned char *source = find_section(&parsed,
                                                   get_u32(relocation + 12));
        const unsigned char *target = find_section(&parsed,
                                                   get_u32(relocation + 16));
        uint64_t source_offset = get_u64(relocation + 48);
        uint64_t region_size = get_u32(relocation + 20);
        uint64_t patch_offset;

        if (!validate_format(relocation) || source == NULL || target == NULL ||
            source_offset > get_u64(source + 24) ||
            region_size > get_u64(source + 24) - source_offset ||
            get_u64(relocation + 56) >
                    (get_u64(target + 32) ? get_u64(target + 32)
                                          : get_u64(target + 24)) ||
            !add_u64(get_u64(source + 16), source_offset, &patch_offset)) {
            return 0;
        }
        for (j = 0; j < i; j++) {
            const unsigned char *other =
                    parsed.relocations + (size_t)j * CACHE_RELOCATION_SIZE;
            const unsigned char *other_source =
                    find_section(&parsed, get_u32(other + 12));
            uint64_t other_offset;
            if (!add_u64(get_u64(other_source + 16), get_u64(other + 48),
                         &other_offset) ||
                ranges_overlap(patch_offset, region_size, other_offset,
                               get_u32(other + 20))) {
                return 0;
            }
        }
    }

    *cache = parsed;
    return 1;
}

static uint64_t low_mask(unsigned bits) {
    return bits == 64 ? UINT64_MAX : (UINT64_C(1) << bits) - 1;
}

static int encode_offset(const unsigned char *relocation,
                         uint64_t value,
                         uint64_t *encoded) {
    uint32_t format = get_u32(relocation + 4);
    unsigned value_size = get_u32(relocation + 28);
    unsigned bit_count = get_u32(relocation + 32);
    unsigned bit_shift = get_u32(relocation + 36);
    unsigned discard_lsb = get_u32(relocation + 40);
    uint64_t discarded_mask = low_mask(discard_lsb);
    uint64_t reduced;

    if ((value & discarded_mask) != 0) {
        return 0;
    }
    if (format == OFFSET_UNSIGNED) {
        reduced = value >> discard_lsb;
        if ((reduced & low_mask(bit_count)) != reduced) {
            return 0;
        }
    } else {
        int64_t signed_value = (int64_t)value;
        int64_t minimum, maximum;
        signed_value >>= discard_lsb;
        if (bit_count == 64) {
            minimum = INT64_MIN;
            maximum = INT64_MAX;
        } else {
            minimum = -(INT64_C(1) << (bit_count - 1));
            maximum = (INT64_C(1) << (bit_count - 1)) - 1;
        }
        if (signed_value < minimum || signed_value > maximum) {
            return 0;
        }
        reduced = (uint64_t)signed_value & low_mask(bit_count);
    }

    *encoded = (reduced & low_mask(bit_count)) << bit_shift;
    return value_size <= 8;
}

static uint64_t load_value(const unsigned char *p, unsigned size) {
    uint64_t value = 0;
    unsigned i;
    for (i = 0; i < size; i++) {
        value |= (uint64_t)p[i] << (i * 8);
    }
    return value;
}

static void store_value(unsigned char *p, unsigned size, uint64_t value) {
    unsigned i;
    for (i = 0; i < size; i++) {
        p[i] = (unsigned char)(value >> (i * 8));
    }
}

int beam_jit_cache_load_aarch64(const BeamJitCache *cache,
                                void *writable_base,
                                const void *executable_base,
                                size_t allocation_size) {
    unsigned char *rw = writable_base;
    uintptr_t rx = (uintptr_t)executable_base;
    uint32_t i;

    if (cache == NULL || writable_base == NULL || executable_base == NULL ||
        cache->image_size > allocation_size) {
        return 0;
    }
    memcpy(rw, cache->image, (size_t)cache->image_size);

    for (i = 0; i < cache->relocation_count; i++) {
        const unsigned char *record =
                cache->relocations + (size_t)i * CACHE_RELOCATION_SIZE;
        const unsigned char *source = find_section(cache, get_u32(record + 12));
        const unsigned char *target = find_section(cache, get_u32(record + 16));
        uint64_t source_at = get_u64(source + 16) + get_u64(record + 48);
        uint64_t target_at = get_u64(target + 16) + get_u64(record + 56);
        uintptr_t address;
        unsigned value_offset = get_u32(record + 24);
        unsigned value_size = get_u32(record + 28);
        unsigned char *word = rw + source_at + value_offset;
        uint64_t encoded, current;

        if (target_at > UINTPTR_MAX - rx) {
            return 0;
        }
        address = rx + (uintptr_t)target_at;
        if (!encode_offset(record, (uint64_t)address, &encoded)) {
            return 0;
        }
        current = load_value(word, value_size);
        if ((current & encoded) != 0) {
            return 0;
        }
        store_value(word, value_size, current | encoded);
    }
    return 1;
}
