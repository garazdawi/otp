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
 * T2-Full tier-2 JIT: high-level IR (HIR) core.
 *
 * This is the P0 skeleton of the tier-2 mid-end described in
 * PLAN/T2FULL/. At this stage the directory only carries the build
 * wiring; the SSA IR, type lattice, and BEAM->SSA builder are added by
 * the following work-order commits. Nothing here is reachable unless a
 * tier-2 boot flag / environment variable is set, so default builds
 * carry zero cost.
 */

#ifndef _JIT_T2_HIR_HPP
#define _JIT_T2_HIR_HPP

namespace erts_t2 {

/* Placeholder so the translation unit is non-empty and the build wiring
 * (Makefile object list, include path, compile-db) can be exercised
 * before the IR lands. Removed once the HIR core is introduced. */
void hir_p0_placeholder(void);

} /* namespace erts_t2 */

#endif /* _JIT_T2_HIR_HPP */
