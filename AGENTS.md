<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2026. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->

# AGENTS.md

Orientation for coding agents, and for anyone who wants the short form. This
file is a map and a checklist. The long form lives in [HOWTO/](HOWTO/) and
[CONTRIBUTING.md](CONTRIBUTING.md), and this file links there rather than
repeating them.

## Ground rules

1. **`export ERL_TOP=$(pwd)` in every shell.** Most of the make system needs it.
   If you are an agent whose environment does not survive from one command to
   the next, set it in the same command. A missing `ERL_TOP` usually surfaces as
   a confusing "No rule to make target".
2. **Push to your own fork, never to `erlang/otp`.** Name the remote explicitly;
   do not rely on what a bare `git push` will do.
3. **Never work directly on `maint` or `master`.** Bug fixes are based on
   `maint`, new features on `master`. See [CONTRIBUTING.md](CONTRIBUTING.md).
4. **Do not include updates to `bootstrap/` or `erts/preloaded/ebin/` in a pull
   request.** They are committed binaries and cannot be reviewed. You may have
   to regenerate them while developing; drop the commit before submitting.
5. **Do not merge `maint` or `master` into your branch.** Rebase.
6. **Run the checks locally before pushing.** A Github Actions round trip is
   roughly half an hour; the same checks take minutes locally.
7. **Report what you actually ran.** A build that used a stale bootstrap, a
   stale PLT or an emulator of the wrong type has not validated anything. Say so
   rather than reporting a pass.

## Where things are

| Path | What it is |
| --- | --- |
| `erts/emulator/` | The virtual machine. `beam/` interpreter and BIFs, `beam/jit/` BeamAsm |
| `erts/preloaded/src/` | Erlang code compiled into the VM |
| `lib/$APP/{src,test,doc}` | The applications |
| `erts/emulator/test/`, `erts/test/` | Emulator and system test suites |
| `bootstrap/` | Committed BEAM files used to compile Erlang/OTP |
| `make/` | Shared make rules. `make/test_target_script.sh` implements `make test` |
| `scripts/` | `otp_build_check`, `license-header.es`, `run-dialyzer`, `pre-push` |
| `.github/workflows/` | CI. `main.yaml` is the entry point, `reusable-*.yaml` the jobs |
| `HOWTO/` | The documentation. Start with [DEVELOPMENT.md](HOWTO/DEVELOPMENT.md) |

## Build

```bash
export ERL_TOP=$(pwd)
./configure          # ./otp_build configure on Windows
make
```

* `make emulator` builds erts and its tools only, which is the usual inner loop
  for VM work. `make $APP` builds one application but not its dependencies, so
  build everything once first.
* Re-run `./configure` after changing a `Makefile`, and `./otp_build
  update_configure` after changing a `configure.ac` or an `.m4` file.
* `export ERLC_USE_SERVER=true` and `./otp_build boot -t` make development
  builds considerably faster. See
  [Faster builds](HOWTO/DEVELOPMENT.md#faster-builds).
* When something stops making sense after switching branches, `git clean -Xfdq`
  and build again.
* For debug, asan, lcnt and valgrind emulators see
  [Types and Flavors](HOWTO/DEVELOPMENT.md#types-and-flavors). Note that an
  unrecognized `TYPE` is silently treated as `opt`, and that a plain `make` only
  rebuilds the default type.

**Before you debug a failure that makes no sense against the source, read
[When the build lies to you](HOWTO/DEVELOPMENT.md#when-the-build-lies-to-you).**
Stale objects after a header change, a stale primary bootstrap and stale
non-default emulators all produce builds that pass without testing what you
think they test.

## Test

```bash
make stdlib_test                                            # one application
make stdlib_test ARGS="-suite lists_SUITE"                  # one suite
make emulator_test ARGS="-suite binary_SUITE -case deep_bitstr_lists"
make stdlib_test ARGS="-suite lists_SUITE -repeat 25"       # soak a flaky case
ERL_ARGS="+hmqd off_heap" make emulator_test                # extra emulator flags
```

Do not call `ct_run` directly; `make test` sets up the test environment first.
Everything in `ARGS` is passed to `ct_run` verbatim. This is the supported way
to run the tests and is documented in [TESTING.md](HOWTO/TESTING.md).

Read the results in `lib/$APP/make_test_dir/ct_logs/index.html`, or
`erts/emulator/make_test_dir/ct_logs/index.html` for `make emulator_test`.
`make test` prints the link when it finishes.

Some applications release the whole system before testing, which is slow to
iterate against, and `TEST_NEEDS_RELEASE=false` skips that while you work on a
single case. See
[Running test cases](HOWTO/DEVELOPMENT.md#running-test-cases) for that, for the
`ARGS` environment variable and for how `ERL_AFLAGS` reaches peer nodes. Those
are details of the make system rather than supported interfaces, so check them
against the guide rather than assuming they still work as described.

## Before you push

```bash
./otp_build check                   # docs, links, dialyzer, format, license
./otp_build check --tests $APP      # narrow it while iterating
scripts/license-header.es ci        # not "reuse lint"
make -C erts/emulator format-check  # clang-format, JIT sources only
git diff --check
```

`./otp_build check --help` lists the individual checks. See
[Static analysis](HOWTO/DEVELOPMENT.md#static-analysis) for what each one
corresponds to in CI, and for the `clang-format` version trap.

If you added something else that CI checks, such as a shell script or a
workflow file, run that checker locally too.

## Continuous integration

```bash
gh run list --branch $BRANCH -R $YOUR_GITHUB_USER/otp
gh run view $RUN_ID -R $YOUR_GITHUB_USER/otp --log-failed
gh run download $RUN_ID -R $YOUR_GITHUB_USER/otp -n test_results
gh run rerun $RUN_ID -R $YOUR_GITHUB_USER/otp --failed
```

`gh` defaults to the repository `origin` points at, so pass `-R` when working on
a fork. Compare a failing job against the same job on the parent commit before
concluding that your change caused it. See
[Debugging Github Actions failures](HOWTO/DEVELOPMENT.md#debugging-github-actions-failures).

## Documentation

`make docs`, or `cd lib/$APP && make docs`, and open
`lib/$APP/doc/html/index.html`. Requires a built tree and `ex_doc`
(`./otp_build download_ex_doc`).

Warnings are errors when the repository is fully built, which is why a
documentation change can pass locally and fail in CI. See
[Common documentation build failures](HOWTO/DOCUMENTATION.md#common-documentation-build-failures).

## Commits and pull requests

[CONTRIBUTING.md](CONTRIBUTING.md) is the authority. The points most often
missed:

* Small, self-contained commits that each compile and pass the relevant tests,
  so that `git bisect` works. Changes to different applications go in different
  commits.
* First line at most 72 characters, no trailing period. Explain **why** in the
  body.
* Correct a commit that is under review with `git commit --fixup=<commit>`
  rather than by rewriting history.
* Check `git status` and `git diff --cached` before every commit. Note that
  `git checkout <commit> -- <path>` stages what it restores.
* Write a test case that fails before the fix and passes after it.

### Disclosing AI assistance

*This is a proposal and not yet project policy.* Following the convention used
by the Linux kernel, a commit or pull request description written with the help
of an AI assistant carries a single trailer naming the model:

```text
Assisted-by: <Assistant>:<model-id>
```

`Signed-off-by` certifies the Developer Certificate of Origin and belongs to the
human submitter; it must not be added on an assistant's behalf, and neither must
`Co-authored-by` or `Co-developed-by`. The human submitter remains responsible
for reviewing the code and owning the contribution.

## Measuring performance

Benchmark and profile an optimized build, not a debug one, and confirm which
emulator you are running before trusting a number. Measure the unchanged tree
twice before attributing a difference to a change; allocator and garbage
collection timing is noisy. When reporting a result, say what it does not cover.
