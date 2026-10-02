# 
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

ifdef BOOTSTRAP
EBIN=$(BOOTSTRAP_TOP)/lib/erts/ebin
else
EBIN=$(ERL_TOP)/lib/erts/ebin
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

STATIC_EBIN=../ebin

include $(ERL_TOP)/erts/vsn.mk
include $(ERL_TOP)/lib/kernel/vsn.mk

RELSYSDIR = $(RELEASE_PATH)/lib/erts-$(VSN)
# not $(RELEASE_PATH)/erts-$(VSN)/preloaded

include modules.mk

ERL_FILES= $(ERTS_ERL_MODULES:%=%.erl)
BEAM_FILES= $(ERTS_BEAM_MODULES:%=%.S)
STUBS_FILES= $(ERTS_BEAM_MODULES:%=%.erl)

TARGET_FILES = $(ERTS_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
    $(APP_TARGET)
STATIC_TARGET_FILES = $(ERTS_MODULES:%=$(STATIC_EBIN)/%.$(EMULATOR))

APP_FILE= erts.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

COMMA=,
APP_MODS_W_COMMA= $(foreach mod, $(ERTS_ERL_MODULES), $(mod) $(COMMA))
APP_MODULES= $(wordlist 2, $(words $(APP_MODS_W_COMMA)), ignore_me $(APP_MODS_W_COMMA))
PRE_LOADED_W_COMMA= $(foreach mod, $(PRE_LOADED_MODULES), $(mod) $(COMMA))
APP_PRE_LOADED_MODULES= $(wordlist 2, $(words $(PRE_LOADED_W_COMMA)), ignore_me $(PRE_LOADED_W_COMMA))

KERNEL_SRC=$(ERL_TOP)/lib/kernel/src
KERNEL_INCLUDE=$(ERL_TOP)/lib/kernel/include
STDLIB_INCLUDE=$(ERL_TOP)/lib/stdlib/include

ERL_COMPILE_FLAGS += -I$(KERNEL_SRC) -I$(KERNEL_INCLUDE) +nowarn_deprecated_catch

ifeq ($(ERL_DETERMINISTIC),yes)
	ERL_COMPILE_FLAGS += +deterministic
endif

DIA_PLT      = erts-preloaded.plt
DIA_ANALYSIS = $(basename $(DIA_PLT)).dialyzer_analysis
ifeq ($(DIAW_EH),true)
DIA_WARNINGS += -Werror_handling
endif
ifeq ($(DIAW_US),true)
DIA_WARNINGS += -Wunderspecs
endif
ifeq ($(DIAW_UR),true)
DIA_WARNINGS += -Wunmatched_returns
endif

debug opt: $(TARGET_FILES)

$(EBIN):
	$(V_at)mkdir -p "$@"

$(TARGET_FILES): | $(EBIN)

clean:
	rm -rf $(EBIN)

copy:
	rm -f $(STATIC_EBIN)/*.beam
	cp $(foreach beam, $(PRE_LOADED_MODULES),$(EBIN)/$(beam).beam) $(STATIC_EBIN)
	$(V_ERL) -noshell -eval "[begin {ok, Bin} = file:read_file(File), {ok, {_, Stripped}} = beam_lib:strip(Bin), ok = file:write_file(File, zlib:gunzip(Stripped)) end || File <- filelib:wildcard(\"$(STATIC_EBIN)/*.beam\")]." -s init stop

$(APP_TARGET): $(APP_SRC) $(ERL_TOP)/erts/vsn.mk Makefile | $(EBIN)
	$(vsn_verbose)sed \
		-e 's;%VSN%;$(VSN);' \
		-e 's;%MODULES%;$(APP_MODULES);' \
		-e 's;%PRE_LOADED_MODULES%;$(APP_PRE_LOADED_MODULES);' \
		$< > $@

#
# Combine a BEAM assembly script file a stub Erlang file into a BEAM file.
# See add_abstract_chunk script.
#

prim_eval.abstr: prim_eval.erl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) -o$(dir $@) +dabstr $<

$(EBIN)/prim_eval.beam: prim_eval.S prim_eval.abstr
	$(gen_verbose)
	$(V_at)$(ERLC) $(ERL_COMPILE_FLAGS) -o$(dir $@) $<
	$(V_at)$(ESCRIPT) add_abstract_code $@ prim_eval.abstr || (rm $@; exit 1)


include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: $(TARGET_FILES)
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(BEAM_FILES) $(STUBS_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:


list_preloaded:
	@echo $(PRE_LOADED_MODULES)

dclean:
	rm -f $(DIA_PLT)
	rm -f $(DIA_ANALYSIS)

dialyzer_plt: $(DIA_PLT)

$(DIA_PLT): $(ERL_FILES)
	@echo "Building ($(basename $(DIA_PLT))) plt file"
	@dialyzer --build_plt \
                  --output_plt $@ \
                  -r ../ebin \
                     ../../../lib/kernel/ebin \
                     ../../../lib/stdlib/ebin \
                     ../../../lib/crypto/ebin \
                     ../../../lib/compiler/ebin \
                  --output $(DIA_ANALYSIS) \
                  --verbose

dialyzer: $(DIA_PLT)
	@echo "Running dialyzer on $(basename $(DIA_PLT))"
	@dialyzer --plt $< \
                  ../ebin \
                  $(DIA_WARNINGS) \
                  --verbose
DEP_REL_TOP=../../../lib/
include $(ERL_TOP)/make/dep.mk
