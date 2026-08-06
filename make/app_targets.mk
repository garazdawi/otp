# 
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
#

APPLICATION ?= $(basename $(notdir $(PWD)))

.PHONY: test info gclean dialyzer dialyzer_plt dclean

ifndef NO_TEST_TARGET
test:
	TEST_NEEDS_RELEASE=$(TEST_NEEDS_RELEASE) TYPE=$(TYPE) MAKE="$(MAKE)" \
	  $(ERL_TOP)/make/test_target_script.sh $(ERL_TOP)
endif

# Turn the coverage collected by `make test COVERAGE=yes` into reports:
# the per-testcase BEAM line coverage into an LCOV tracefile + interactive
# attribution report, and, if the tests ran on a clangcov emulator, the
# native C/JIT coverage into an LCOV tracefile (see HOWTO/DEVELOPMENT.md).
COVERAGE_DIR ?= make_test_dir/coverage
.PHONY: coverage_report
coverage_report:
	@had=no; \
	if [ -f "$(COVERAGE_DIR)/coverage.manifest" ]; then \
	  $(ERL_TOP)/bin/escript $(ERL_TOP)/lib/common_test/ebin/ct_cover_to_lcov.beam \
	    --manifest "$(COVERAGE_DIR)/coverage.manifest" \
	    "$(COVERAGE_DIR)/coverage.info" "$(COVERAGE_DIR)" && \
	  $(ERL_TOP)/bin/escript $(ERL_TOP)/lib/common_test/ebin/ct_cover_to_html.beam \
	    --manifest "$(COVERAGE_DIR)/coverage.manifest" --max-per-line 5 \
	    "$(COVERAGE_DIR)/html" "$(COVERAGE_DIR)"; \
	  echo "BEAM line LCOV:      $(COVERAGE_DIR)/coverage.info"; \
	  echo "Attribution report:  $(COVERAGE_DIR)/html/index.html"; \
	  had=yes; \
	fi; \
	if [ -d "$(COVERAGE_DIR)/native" ]; then \
	  $(ERL_TOP)/make/native_cov_to_lcov.sh "$(COVERAGE_DIR)/native" \
	    "$(ERL_TOP)/bin/$(TARGET)/beam.clangcov.jit" \
	    "$(COVERAGE_DIR)/native.info"; \
	  if [ -f "$(COVERAGE_DIR)/native.info" ]; then \
	    echo "Native C/JIT LCOV:   $(COVERAGE_DIR)/native.info"; had=yes; \
	  fi; \
	fi; \
	if [ "$$had" = no ]; then \
	  echo "No coverage data in $(COVERAGE_DIR). Run 'make test COVERAGE=yes' first."; \
	  exit 1; \
	fi

docs: $(filter src java_src, $(SUB_DIRECTORIES))

info:
	@echo "$(APPLICATION)_VSN:   $(VSN)"
	@echo "APP_VSN:   $(APP_VSN)"
	@echo ""
	@echo "DIA_PLT:      $(DIA_PLT)"
	@echo "DIA_ANALYSIS: $(DIA_ANALYSIS)"
	@echo ""

gclean: 
	git clean -fXd


DIA_DEFAULT_PLT_APPS = erts kernel stdlib crypto compiler $(APPLICATION)
DIA_PLT_DIR  = ./priv/plt
DIA_PLT      = $(DIA_PLT_DIR)/$(APPLICATION).plt
DIA_ANALYSIS = $(basename $(DIA_PLT)).dialyzer_analysis
DIA_RUNTIME_DEPS = $(shell erl -noinput -eval '{ok, [{_, _, Keys}]} = file:consult(filelib:wildcard("ebin/*.app")), Deps = [hd(string:split(Deps, "-")) || Deps <- proplists:get_value(runtime_dependencies, Keys)], io:format("~ts",[lists:join(" ", Deps)]), init:stop().')

dialyzer_plt: $(DIA_PLT)

$(DIA_PLT_DIR):
	@mkdir -p $@

$(DIA_PLT): $(DIA_PLT_DIR)
	@echo "Building $(APPLICATION) plt file"
	$(V_at)dialyzer --build_plt \
                  --output_plt $@ \
		  -Wno_unknown \
		  --apps $(sort $(DIA_PLT_APPS) $(DIA_RUNTIME_DEPS) $(DIA_DEFAULT_PLT_APPS)) \
		  --output $(DIA_ANALYSIS) \
                  --verbose

dialyzer: $(DIA_PLT)
	@echo "Running dialyzer on $(APPLICATION)"
	$(V_at)dialyzer --plt $< \
                  ../$(APPLICATION)/ebin \
                  --verbose

dclean:
	rm -f $(DIA_PLT)
	rm -f $(DIA_ANALYSIS)
