# 
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2026. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%

PRE_LOADED_MODULES = \
	erl_prim_loader \
	init \
	prim_buffer \
	prim_file \
	erl_init \
	erts_code_purger \
	erlang \
	erts_internal \
	erts_literal_area_collector \
	erts_trace_cleaner \
	erts_dirty_process_signal_handler

ESOCK_ERL_MODULES = \
	socket_registry \
	prim_socket \
	prim_net

ERTS_ERL_MODULES = \
	$(PRE_LOADED_MODULES) \
	$(ESOCK_ERL_MODULES) \
	atomics \
	counters \
	prim_inet \
	zlib \
	prim_zip \
	erl_tracer \
	persistent_term

ERTS_BEAM_MODULES = erl_eval

ERTS_MODULES = $(ERTS_ERL_MODULES) $(ERTS_BEAM_MODULES)
