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
 * T2-Full tier-2 JIT: the type lattice.
 *
 * A C++ port of lib/compiler/src/beam_types.hrl (see PLAN/T2/01 §5.3),
 * used both to seed SSA values from the BEAM file's type chunk (§5.4
 * source 1) and as the lattice for the in-tier forward-dataflow pass
 * (source 3).
 *
 * The type-union bit constants are reused directly from beam_types.h
 * rather than re-encoded, so the AOT and tier-2 representations cannot
 * drift; a static_assert bridge below pins the assumption.
 */

#ifndef _JIT_T2_TYPES_HPP
#define _JIT_T2_TYPES_HPP

#include <cstdint>
#include <algorithm>

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "beam_types.h"
}

namespace erts_t2 {

    /* Limits ported from beam_types.hrl. Kept as named constants so the
     * lattice refinements match the AOT lattice's granularity. */
    constexpr int T2_ATOM_SET_SIZE = 5;        /* ?ATOM_SET_SIZE */
    constexpr int T2_TUPLE_ELEMENT_LIMIT = 12; /* ?TUPLE_ELEMENT_LIMIT */
    constexpr int T2_TUPLE_SET_LIMIT = 12;     /* ?TUPLE_SET_LIMIT */

    /* The bit width of the type union. beam_types.h defines BEAM_TYPE_ANY as
     * all bits below 1<<12 set. We keep the whole union in a 16-bit field. */
    static_assert(BEAM_TYPE_ANY == ((1 << 12) - 1),
                  "T2 lattice assumes a 12-bit BEAM type union; keep "
                  "t2_types.hpp in sync with beam_types.h");
    static_assert(BEAM_TYPES_VERSION == 3,
                  "T2 lattice ported against BEAM_TYPES_VERSION 3");

    /*
     * A lattice element. `Top` (any) is type_union == BEAM_TYPE_ANY with no
     * refinements; `Bottom` (none, i.e. unreachable / contradiction) is
     * type_union == BEAM_TYPE_NONE.
     *
     * P0 tracks the type union plus the two refinements the type chunk can
     * carry: the integer range and the bitstring unit. The atom-set and
     * tuple-shape refinements of the reference lattice are reserved for the
     * dataflow phases; they are represented conservatively here (any) so
     * meet/join stay sound until they are populated.
     */
    struct T2Type {
        uint16_t type_union; /* BEAM_TYPE_* bitset */

        bool has_min;
        bool has_max;
        Sint64 min;
        Sint64 max;

        uint8_t unit; /* bitstring unit; 0 == unknown */

        /* ---- constructors ------------------------------------------------ */

        static constexpr T2Type any() {
            return T2Type{(uint16_t)BEAM_TYPE_ANY, false, false, 0, 0, 0};
        }

        static constexpr T2Type none() {
            return T2Type{(uint16_t)BEAM_TYPE_NONE, false, false, 0, 0, 0};
        }

        static constexpr T2Type of(int type_union_) {
            return T2Type{(uint16_t)type_union_, false, false, 0, 0, 0};
        }

        static T2Type integer(Sint64 lo, Sint64 hi) {
            return T2Type{(uint16_t)BEAM_TYPE_INTEGER, true, true, lo, hi, 0};
        }

        /* Seed a lattice element from a decoded type-chunk entry. */
        static T2Type from_beam(const BeamType &bt) {
            T2Type t = any();
            t.type_union = (uint16_t)(bt.type_union & BEAM_TYPE_ANY);
            if (bt.metadata_flags & BEAM_TYPE_HAS_LOWER_BOUND) {
                t.has_min = true;
                t.min = bt.min;
            }
            if (bt.metadata_flags & BEAM_TYPE_HAS_UPPER_BOUND) {
                t.has_max = true;
                t.max = bt.max;
            }
            if (bt.metadata_flags & BEAM_TYPE_HAS_UNIT) {
                t.unit = bt.size_unit;
            }
            return t;
        }

        /* ---- predicates -------------------------------------------------- */

        bool is_any() const {
            return type_union == BEAM_TYPE_ANY && !has_min && !has_max && !unit;
        }

        bool is_none() const {
            return type_union == BEAM_TYPE_NONE;
        }

        /* True iff this element is exactly one primitive type. */
        bool is_single() const {
            return type_union != 0 && (type_union & (type_union - 1)) == 0;
        }

        bool has(int bits) const {
            return (type_union & bits) != 0;
        }

        /* Exactly the integer type. (Named to avoid the is_integer() term
         * macro from erl_term.h, which may be in scope at inclusion.) */
        bool integer_only() const {
            return type_union == BEAM_TYPE_INTEGER;
        }

        bool equals(const T2Type &o) const {
            return type_union == o.type_union && has_min == o.has_min &&
                   has_max == o.has_max && (!has_min || min == o.min) &&
                   (!has_max || max == o.max) && unit == o.unit;
        }

        /* ---- lattice operations ------------------------------------------ */

        /* meet == intersection == narrowing (more information, moves down). */
        T2Type meet(const T2Type &o) const {
            T2Type r;
            r.type_union = (uint16_t)(type_union & o.type_union);
            if (r.type_union == BEAM_TYPE_NONE) {
                return none();
            }

            /* Tightest lower bound wins. */
            r.has_min = false;
            r.min = 0;
            if (has_min && o.has_min) {
                r.has_min = true;
                r.min = std::max(min, o.min);
            } else if (has_min) {
                r.has_min = true;
                r.min = min;
            } else if (o.has_min) {
                r.has_min = true;
                r.min = o.min;
            }

            r.has_max = false;
            r.max = 0;
            if (has_max && o.has_max) {
                r.has_max = true;
                r.max = std::min(max, o.max);
            } else if (has_max) {
                r.has_max = true;
                r.max = max;
            } else if (o.has_max) {
                r.has_max = true;
                r.max = o.max;
            }

            /* An empty integer range means the integer part is impossible. */
            if (r.has_min && r.has_max && r.min > r.max) {
                r.type_union &= (uint16_t)~BEAM_TYPE_INTEGER;
                r.has_min = r.has_max = false;
                if (r.type_union == BEAM_TYPE_NONE) {
                    return none();
                }
            }

            /* Unit: keep a set unit; conflicting units keep the finer one. */
            if (unit && o.unit) {
                r.unit = (uint8_t)std::max(unit, o.unit);
            } else {
                r.unit = unit ? unit : o.unit;
            }

            return r;
        }

        /* join == union == widening (less information, moves up). */
        T2Type join(const T2Type &o) const {
            if (is_none()) {
                return o;
            }
            if (o.is_none()) {
                return *this;
            }

            T2Type r;
            r.type_union = (uint16_t)(type_union | o.type_union);

            /* A bound survives a join only if both sides bound it. */
            r.has_min = has_min && o.has_min;
            r.min = r.has_min ? std::min(min, o.min) : 0;
            r.has_max = has_max && o.has_max;
            r.max = r.has_max ? std::max(max, o.max) : 0;

            r.unit = (unit == o.unit) ? unit : 0;

            return r;
        }
    };

} /* namespace erts_t2 */

#endif /* _JIT_T2_TYPES_HPP */
