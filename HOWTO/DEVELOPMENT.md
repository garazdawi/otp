<!--
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->

# Developing Erlang/OTP

The Erlang/OTP development repository is quite large and the make system
contains a lot of functionality to help when developing. This howto
will try to showcase the most important features of the make system.

The guide is mostly aimed towards development on a Unix platform, but
most things should work also work using WSL on Windows. The guide also
assumes that you are working in the git repository. Many of the
scripts and tools described here are not available in the prebuilt tar
archive that you can download for each release.

*WARNING*: Only some of APIs mentioned in this guide are supported. This
means that they may be removed or changed without prior notice, so do
not depend on them in CI. For supported make targets see the
[Install howto](INSTALL.md) and the [Testing howto](TESTING.md).

The make system is not always as robust as one might like, so if for
any reason something does not work, try doing a `git clean -Xfdq` and
start from the beginning again. This normally only needs to be done when
you jump in between different git branches, but it can a good thing to
keep in mind whenever things do not work as you expect them to.

*NOTE*: This instructions may vary for different versions of Erlang/OTP,
so make sure to read the instructions for the version that you are working
with.

1. [Short version](#short-version)
2. [Preparations](#preparations)
    1. [Faster builds](#faster-builds)
3. [Configuring](#configuring)
    1. [Help](#help)
4. [Building and testing](#building-and-testing)
    1. [Build and test a specific application](#build-and-test-a-specific-application)
    2. [Preloaded and Primary Bootstrap](#preloaded-and-primary-bootstrap)
    3. [Types and Flavors](#types-and-Flavors)
    4. [cerl](#cerl)
    5. [Static analysis](#static-analysis)
5. [When the build lies to you](#when-the-build-lies-to-you)
6. [Running test cases](#running-test-cases)
    1. [Where the test logs end up](#where-the-test-logs-end-up)
    2. [Repeating and targeting test cases](#repeating-and-targeting-test-cases)
    3. [Testing without releasing](#testing-without-releasing)
7. [Writing and building documentation](#writing-and-building-documentation)
    1. [Validating documentation](#validating-documentation)
8. [Github Actions](#github-actions)
    1. [Debugging github actions failures](#debugging-github-actions-failures)
9. [Using Docker](#using-docker)
    1. [Gidpod.io or VSCode dev container](#gitpod-io-or-vscode-dev-container)

## Short version

First make sure you have done all [preparations](#preparations) then
do this:

```bash
git clone -b maint git@github.com:erlang/otp
cd otp && export ERL_TOP=`pwd`
./otp_build configure && make
```

When you have done changes, added tests or updated documentation, build and test like this:

```bash
cd lib/$APPLICATION_NAME
make            # Rebuid application
make test       # Run application tests
make dialyzer   # Run dialyzer
make docs       # Build the docs
```

Then enable [Github Actions](#github-actions) and push the changes to your fork
of Erlang/OTP to check that you have not missed anything.

## Preparations

Before you start working you need to clone the Erlang/OTP git repository
and install any dependencies that you do not have. See
[Required Utilities](INSTALL.md#required-utilities) and
[Optional Utilities](INSTALL.md#optional-utilities) in [INSTALL.md](INSTALL.md)
for a list of utilities to install. (Windows has its own [INSTALL Guide](INSTALL-WIN32.md)
with its own [Required Utilities](INSTALL-WIN32.md#tools-you-need-and-their-environment)).

Then you need to set `ERL_TOP` to point at the repository you are developing in.
Not all make commands needs this environment variable set, but many do so it is
good to get into the habit of always setting it.

```bash
cd /path/to/repository/otp
export ERL_TOP=`pwd`
```

Make sure that you have read the [Contributing to Erlang/OTP](../CONTRIBUTING.md)
guide if you intend to make a contribution to Erlang/OTP.

### Faster builds

Both `configure` and `make` take advantage of running in parallel if told to,
so in order to speed up your development environment make sure to set:

```bash
## Change N to be at least the number of cores or hyper-threads available
export MAKEFLAGS=-jN
```

The Erlang compiler can be run using a [Compile Server](https://www.erlang.org/doc/man/erlc.html#compile-server),
this can cut from the total build time of Erlang/OTP by quite a lot,
especially if you have a relatively slow machine.
To enable set this environment variable:

```bash
export ERLC_USE_SERVER=true
```

Re-building all application in Erlang/OTP can take a while so it is possible
to build only a subset of the applications. This is done by setting either
`OTP_SMALL_BUILD` or `OTP_TINY_BUILD` to `true` when doing make at the top
level. However, the simplest way is probably to just use the `./otp_build`
wrapper that takes the options `-t` (tiny) or `-a` (all) and defaults to
a small build.

```bash
# You need to have done ./configure before calling make or boot.
OTP_TINY_BUILD=true make      ## Equivalent to ./otp_build boot -t
OTP_SMALL_BUILD=true make     ## Equivalent to ./otp_build boot
./otp_build boot -a           ## Equivalent to make
```

## Configuring

You run configure by issuing the command:

```bash
./otp_build configure
```

On all operating systems except Windows you can also just run:

```bash
./configure
```

If you change any `Makefile`s you will need to re-run configure.
If you update any `configure.ac` scripts you need to
[update the configure scripts](INSTALL.md#updating-configure-scripts).

### Help

The toplevel configure will give help about the features that it provides.
To get a full list of all features you need to use:

```bash
./configure --help=r
```

There is documentation for what most of the options mean in the
[INSTALL.md](INSTALL.md#Configuring) howto.

## Building and testing

After you have done configure, you can do

```bash
make
```

on the top of this repository. That will compile all of Erlang/OTP.

You can also build a specific application:

```bash
make stdlib
make common_test
```

These make commands do not manage any dependencies, so if an application needs
something from another you need to make sure that it is built. It is therefore
good practice to first build all of Erlang/OTP and then build just the one that
you are updating.

You can also run tests from the top:

```bash
make test                # Run all tests, takes a **very** long time
make stdlib_test         # Run only stdlib tests, takes less time
                         # Run only lists_SUITE, takes even less time
make stdlib_test ARGS="-suite lists_SUITE"
                         # Run only member testcase in lists_SUITE
make stdlib_test ARGS="-suite lists_SUITE -case member"
```

See [ct_run](https://www.erlang.org/doc/man/ct_run.html#) for a list of all options
that you can pass to ARGS.

Most of the above targets also works for a "phony" target called `emulator` that
represents erts and all its tools. So you can do this:

```bash
make emulator            # Build erts, epmd etc
make emulator_test       # Run all emulator tests
```

If you want to pass a run-time flag to the emulator running the tests you can
use the `ERL_ARGS` flags to `make test`. For example if you want to run tests
using [off heap message queue data](https://www.erlang.org/doc/man/erlang.html#process_flag_message_queue_data)
for all process you would do this:

```bash
ERL_ARGS="+hmqd off_heap" make emulator_test
```

You can run static analysis test:

```bash
./otp_build check
```

See `./otp_build check --help` for details on how to use it and what it does.

### Build and test a specific application

You can also build the application from within itself. Like this:

```bash
cd lib/stdlib && make
```

Each application has a bunch of make targets that you can use.

```bash
make                                # build all source for this application
make test                           # run all tests for this application
make test ARGS="-suite lists_SUITE" # run the lists_SUITE tests
make dialyzer                       # run dialyzer for this application
make docs                           # build all docs for this application
make docs DOC_TARGETS="html"        # build html docs for this application
```

Open `doc/html/index.html` in your favorite web browser to view the documentation.

### Preloaded and Primary Bootstrap

The Erlang code loader and compiler are written in Erlang, so in order to
[bootstrap](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
the system a number of compiled `.beam` files are committed into the
Erlang/OTP git repository.

The Erlang code located in [erts/preloaded/src](../erts/preloaded/src)
is compiled into the VM and used to load enough code so that the code
loader in the `kernel` application can be loaded. If you update any of
that code you need to do a special preloaded update for the changes to
take effect. This is done like this:

```bash
./otp_build update_preloaded [--no-commit]
make  # Need to rebuild system after the preloaded has been updated
```

You need to have a working Erlang compiler in your path for this to work.
In order to be able to compile the Erlang/OTP source code, there also needs
to be a basic Erlang compiler committed into git. This is what is called the
primary bootstrap. It is quite rare that you need to update this, but if you
are extending the Erlang language and would like to use the new extensions
in the Erlang/OTP source code you it needs to be updated. As an example, when
we added `maps` to Erlang we first needed to have a committed primary bootstrap
that could compile code with maps, before we actually could use maps anywhere.
To update the primary bootstrap you do like this:

```bash
./otp_build update_primary [--no-commit]
```

There is one situation where you have to update the primary bootstrap even
though you are not extending the language: when you want to build Erlang/OTP
*with* a compiler change in order to validate it. Until the bootstrap is
updated, `make` uses the committed compiler from before your change, so the
build succeeds without your change ever having been used. The same applies to
[erts/preloaded/src](../erts/preloaded/src) and `update_preloaded`. Use
`--no-commit` and drop the change again before you submit.

*NOTE*: When submitting a PR to Erlang/OTP you will be asked to not include
any commit updating preloaded or the primary bootstrap. This is because we
cannot review the contents of binary files and thus cannot make sure they do
not contain any malicious data.

### Types and Flavors

Erlang can be built using different types and flavors. Mostly the types and
flavors change how the Erlang VM itself is built, but some also effect how
application are built. Some of the types/flavors are:

* Types
  * opt (default)
  * debug
  * lcnt
  * valgrind
  * asan
  * gcov
* Flavor
  * emu
  * jit (default if available)

To build using a type and or flavor you just pass it as a variable to make.
For example:

```bash
make TYPE=debug
make FLAVOR=emu
make TYPE=lcnt FLAVOR=emu
```

As you can see it is possible to combine type and flavor to create many different
versions of Erlang. You can then run these different versions by passing the
`-emu_type` and/or `-emu_flavor` flags to `erl`. That is:

```bash
erl -emu_type lcnt
erl -emu_flavor emu -emu_type debug
```

When running valgrind, asan or gcov those tools create special output files that
need to be processed. To work with these files there is a special `erl` program
called `cerl` that is only available in the source tree. You can read more about
it in the [cerl section](#cerl) later in this guide.

If you want to run the tests with a special flavor or type, the easiest way to
do that is by setting the TYPE or FLAVOR when calling make. For example if you
want to run the emulator tests using the debug emulator you can do it like this:

```bash
make emulator_test TYPE=debug
```

*NOTE*: Before you run tests using a TYPE or FLAVOR you need to build the **entire**
Erlang/OTP repo using that TYPE or FLAVOR. That is `make TYPE=debug` for the example
above.

*NOTE*: Each type and flavor is a separate binary, and a later plain `make` only
rebuilds the default one, so the debug or asan emulator you built earlier can be
older than your source tree without `make` saying anything. Before you rely on a
debug assertion or on a measurement, check what you are actually running:

```bash
bin/cerl -debug -noshell \
  -eval 'io:format("~p ~p~n",[erlang:system_info(build_type),
                              erlang:system_info(emu_flavor)]),halt().'
```

### cerl

`cerl` is a program available in `$ERL_TOP/bin/` that has a number of features
useful when developing the Erlang run-time system. It works just as normal `erl`,
but accepts a couple of extra command line switches. Any other command line arguments
passed to `cerl` will be passed on the Erlang as normal. The extra command line
switches are:

* -debug
  * Start a debug run-time system.
* -lcnt
  * Start a lock count run-time system.
* -valgrind
  * Start valgrind with the correct settings and use the `valgrind` [type](types-and-flavors).
  * Set environment variable `VALGRIND_LOG_XML` to true if want xml valgrind logs.
  * Set environment variable `VALGRIND_LOG_DIR` to where you want valgrind logs.
  * Set environment variable `VALGRIND_MISC_FLAGS` for any extra valgrind flags you want to pass.
* -asan
  * Start [Clang Address Sanitizer](https://clang.llvm.org/docs/AddressSanitizer.html)
    with the correct settings and use the `asan` [type](types-and-flavors).
  * Set environment variable `ASAN_LOG_DIR` to where you want the logs.
  * Set environment variable `ASAN_OPTIONS` for any extra asan options you want to pass.
* -gcov
  * Start a gcov run-time system.
  * Generate results by running: `$ERL_TOP/erts/etc/unix/gcov-gen-html $ERL_TOP <OUTPUT_DIR>`
* -gdb
  * Start an Emacs gdb debugging session. Can be combined with -debug.
* -core /path/to/core/file
  * Start an Emacs gdb debugging session for the core specified.
* -rgdb
  * Start a gdb debugging session in the current terminal. Can be combined with -debug.
* -rcore /path/to/core/file
  * Start a gdb debugging session in the current terminal for the core specified.
* -lldb
  * Start a lldb debugging session in the current terminal.
* -rr
  * Start Erlang under [rr](https://rr-project.org/) to record all events. Can be combined with -debug.
* -rr replay [session]
  * Load a recording session using `rr replay`, if no session is specified the latest run session is loaded.

If you want to run tests using `cerl` (for example if you want to run asan on
the nif_SUITE in emulator) you cannot use the `make test` approach to testing
as that uses `ct_run` under the hood and `ct_run` does not support customizing
the emulator start script. Instead you need to use the approach described in
[Run tests with Address Sanitizer](TESTING.md#run-tests-with-address-sanitizer).


### Static analysis

From the top level of Erlang/OTP you can run:

```bash
./otp_build check
```

This will check that the documentation is correct, that there are no
dialyzer errors and many more things. Run `./otp_build check --help` for the
list of individual checks and the `--no-*` options that turn them off, and use
`--tests` to limit the test suite build check to the applications you touched:

```bash
./otp_build check --tests stdlib
```

Each of these checks corresponds to a [Github Actions](#github-actions) job, and
running them locally takes minutes where a Github Actions round trip takes
roughly half an hour. The individual checks can also be run on their own:

```bash
scripts/license-header.es ci        # license and copyright headers
make -C erts/emulator format-check  # clang-format, JIT sources only
(cd lib/$APPLICATION_NAME && make dialyzer)
git diff --check                    # whitespace errors
```

`make dialyzer` builds a plt in `lib/$APPLICATION_NAME/priv/plt` from the
applications it depends on, and rebuilds it whenever any of those have been
rebuilt. The first run after a full build therefore takes a while.

*NOTE*: The license header check is `scripts/license-header.es ci`, which is
what Github Actions runs. `reuse lint` is a different tool with a different
scope and will report the whole repository as non-compliant. See
[FILE-HEADERS.md](FILE-HEADERS.md).

*NOTE*: Different major versions of `clang-format` disagree about where to break
lines, so formatting with one version and checking with another shows up as
changes to lines you never touched. `make format-check` and `make format`
therefore refuse to run unless `clang-format` is the major version recorded in
`make/clang_format_vsn`, which is the version Github Actions uses. If that
version is not the first one in your path, point at it explicitly:

```bash
make -C erts/emulator format-check CLANG_FORMAT=clang-format-14
```

Note also that `git clang-format` exits with a non-zero status when it modifies
files, which silently ends an `&&` chain.

## When the build lies to you

The make system's dependency tracking is not complete, and a few build
artifacts are committed to git rather than derived from the sources. The result
is a class of problem where the build succeeds but does not build, or does not
test, what you think it does. They are all easier to recognise than to debug, so
they are collected here.

**Stale object files after a header change.** Changing an `enum` member or a
struct layout in a widely included header does not reliably rebuild every object
file that depends on it. Freshly built objects then disagree with stale ones
about numeric values or offsets, which shows up as assertion or crash failures
that are impossible given the source code. If a failure makes no sense when you
read the code, suspect the build before you suspect the code: remove the object
files in question and any pre-compiled headers (`*.gch`), or do
`make clean TYPE=$TYPE FLAVOR=$FLAVOR`, and build again.

**A stale primary bootstrap.** The compiler in `bootstrap/` is what compiles
Erlang/OTP. If you change `lib/compiler` and run `make`, the system is still
compiled by the old committed compiler, so the build passes without ever
exercising your change. See
[Preloaded and Primary Bootstrap](#preloaded-and-primary-bootstrap).

**Stale emulators of another type or flavor.** Each [type and flavor](#types-and-flavors)
is a separate binary. A plain `make` only rebuilds the default one, so your
debug or asan emulator can be days older than your source tree while `make`
reports success. See [Types and Flavors](#types-and-flavors) for how to check
which one you are running. Note that a TYPE that does not exist is rejected, so
this is about a stale build of a real type, not a misspelled one.

**Shadowed executables.** Erlang version managers put their own `erl`, `erlc`
and `ex_doc` ahead of the ones in the source tree. When you mean to use the tree
you are developing in, put it first:

```bash
export PATH=$ERL_TOP/bin:$PATH
```

**A clean compile is not a working build.** A miscompiled module compiles
without complaint and fails when it runs, typically while the build is using it
to compile something else. Check that `bin/erl` still starts before you conclude
that a compiler or emulator change is good.

When more than one of these is in play at once, `git clean -Xfdq` followed by a
full rebuild is usually faster than working out which one you hit.

## Running test cases

There is a detailed description about how to run tests in [TESTING.md](TESTING.md),
which is also where the supported way of running them is described. The rest of
this section covers details of how the make system runs the tests. As with
everything else in this guide, they may change without notice.

### Where the test logs end up

When you run `make test` in the source tree the results are written to a
`make_test_dir` next to the application being tested:

```text
lib/$APPLICATION_NAME/make_test_dir/ct_logs/index.html
erts/emulator/make_test_dir/ct_logs/index.html          # make emulator_test
```

`make test` prints a link to this index when it finishes, both when the tests
pass and when they fail. The logs for an individual suite are one level further
down, in `ct_logs/ct_run.$NODE.$TIMESTAMP/`.

`make_test_dir` is generated and is ignored by git, so nothing in it should be
edited. In particular the test specification found there is a copy; the one to
change is in `lib/$APPLICATION_NAME/test/`.

### Repeating and targeting test cases

`ARGS` is read from the environment as well as from the make command line, so
these are equivalent:

```bash
make stdlib_test ARGS="-suite lists_SUITE"
ARGS="-suite lists_SUITE" make stdlib_test
```

The environment form is useful for targeting a single test case in an
environment where you cannot easily change the command that is run, such as
inside an already built container.

To chase a race you can repeat the whole selection with `ct_run`'s `-repeat`
flag. `ct_run` exits with a non-zero status if any iteration failed, so a
successful `make` means every iteration passed:

```bash
make emulator_test ARGS="-suite signal_SUITE -case dirty_signal_handling -repeat 25"
```

Note the difference between `ERL_ARGS` and `ERL_AFLAGS` here. `ERL_ARGS` is
passed to `ct_run` after `-erl_args` and so only reaches the node running the
tests, whereas `ERL_FLAGS` and `ERL_AFLAGS` are picked up from the environment
by every emulator started, including the peer nodes that the suites start
themselves. That is how `TYPE` and `FLAVOR` reach the peers; the make system
appends `-emu_type` and `-emu_flavor` to `ERL_AFLAGS`. It also means that a
suite which needs to control its peers has to pass the flag on the peer's own
command line, which takes precedence.

### Testing without releasing

Some applications have to be tested against a released Erlang/OTP rather than
the source tree, because they test release handling, installation paths or code
upgrade. Those applications set `TEST_NEEDS_RELEASE=true` in their `Makefile`:

```bash
grep -l TEST_NEEDS_RELEASE=true lib/*/Makefile
```

For them `make $APPLICATION_NAME_test` does a full release into `make_test_dir`
first and runs the tests against that, which adds several minutes to every
iteration.

While working on a single test case you can skip that step:

```bash
make stdlib_test TEST_NEEDS_RELEASE=false ARGS="-suite lists_SUITE -case member"
```

The tests then run against the source tree instead, which is a great deal
faster. Test cases that genuinely need a released system will fail or be skipped
when you do this, so run them again without the override before concluding that
a change works.

## Writing and building documentation

Most of the Erlang/OTP documentation is written using markdown, either directly
in the module implementing the functionality, or as `.md` files located in
`lib/$APPLICATION_NAME/doc`. For more details about how to write documentation see
[Writing Documentation](../system/doc/reference_manual/documentation.md) and the
[Erlang/OTP Documentation HOWTO](DOCUMENTATION.md).

There is also some documentation that is written using [edoc](https://www.erlang.org/doc/man/edoc.html).

In order to build the documentation you need to have [ex_doc](https://hexdocs.pm/ex_doc/readme.html)
installed and available in your path. The simplest way to do that is to download
the [escript for the latest release](https://github.com/elixir-lang/ex_doc/releases/latest) available on github.

To view the documentation you need to build it. *NOTE*: The Erlang/OTP
repository needs to have been [built](#building-and-testing) before you can build
the documentation.

```bash
make docs
```

and then you can view `doc/index.html` in your favourite browser and
make sure that it looks nice.

This takes a while though and to speed up the edit-view cycle you can build the
docs only for a single application. For example:

```bash
cd lib/stdlib && make docs
```

and then view the results at `lib/stdlib/doc/html/index.html`.

### Validating documentation

In order to make sure that the documentation is correct you need to also
validate it. Just building the documentation does not mean that it is
correct.

You need to make sure that there are no broken links in the documentation.
This is done by running `./otp_build check`.

If there are broken links `./otp_build check` will print a list of broken links and anchors, for example:

```text
lib/kernel/doc/html/eep48_chapter.html: Found duplicate anchor see-also
lib/kernel/doc/html/eep48_chapter.html: could not find lib/stdlib/doc/html/shell_docs.html#contents, should it be #content?
```

The analysis is done on the generated html files and prints the source html file on the left
and information about what is wrong on the right. In the examples above the checker has found
that there are two or more html anchors called `see-also` in the `eep48_chapter.html` file
and also that there is a link to an unknown anchor called `#contents` in `shell_docs.html`.

All this validation is also done by [Github Actions](#github-actions).

#### Link checker dependencies

The link checker is written using Elixir, so you will need to install Elixir
version 1.16 or later for it to work. You may also have to build hex from sources,
depending on if the pre-built .beam archive can be used or not. To build latest Elixir
and hex do the following:

```bash
export PATH=$ERL_TOP/bin:$PATH
git clone git@github.com:elixir-lang/elixir
(cd elixir && make)
export PATH=`pwd`/elixir/bin:$PATH
git clone git@github.com:hexpm/hex
(cd hex && mix install)
```

To be sure that the compiled Elixir and Hex always works you want to do the above with the
currently oldest supported Erlang release. However there is no point in using a release older
than Erlang/OTP 27 as that is when elixir was first added as a dependency.

## Github Actions

Erlang/OTP uses [Github Actions](https://github.com/features/actions) as a
preliminary CI to check that nothing fundamental has been broken by the change.

You can enable Github Actions on your own github fork in order to run the tests
before opening a PR to the main repository.

Github Actions does too many checks to list them all but the primary ones are:

* Build on Ubuntu Linux and Windows
* Cross build to Debian Linux on powerpc and iOS
* Build and validate documentation
* Run dialyzer on all of Erlang/OTP
* Run the tests of the changed application

Each run generates a bunch of artifacts. The most important ones are:

* `test_results`
  * An archive containing all the logs from all tests that have been run.
    Navigate to `make_test_dir/ct_logs/index.html` within the archive to
    view the Common Test summary of the tests.
* `otp_win32_installer`
  * A windows installer with the changes you have made.
* `otp_doc_html`
  * The HTML docs with the changes you have made.

### Debugging Github Actions failures

Debugging Github Actions is at best a very time-consuming endevour. So if there
is an error in the build or tests that you don't easily understand I would
recommend that you try to reproduce it locally.

This is of course not always possible, for instance if it only fails on Windows
and you do not have access to a Windows machine, but it may the worth it as the
leadtime of re-running a test is roughly 30 minutes. See the [other sections of
this guide](#developing-erlang-otp) for details on how to build and run tests
locally.

If testcases fail when running Github Actions, it is best to start by inspecting
the logs of the test runs. The logs are attached to the finished run as
`test_results`. You will find more details about why a testcase failed in
the logs.

Using the [Github CLI](https://cli.github.com):

```bash
gh run list --branch $BRANCH -R $YOUR_GITHUB_USER/otp
gh run view $RUN_ID -R $YOUR_GITHUB_USER/otp --json status,conclusion,jobs
gh run view $RUN_ID -R $YOUR_GITHUB_USER/otp --log-failed
gh run download $RUN_ID -R $YOUR_GITHUB_USER/otp -n test_results
gh run rerun $RUN_ID -R $YOUR_GITHUB_USER/otp --failed
```

After downloading `test_results`, open `make_test_dir/ct_logs/index.html` in it.
`gh run rerun --failed` re-runs only the failed jobs and reuses the build from
the original run, which is a lot faster than pushing again.

*NOTE*: `gh` picks a repository based on the git remotes, which for a fork of
Erlang/OTP is not necessarily your own. Pass `-R $YOUR_GITHUB_USER/otp`
explicitly or you may be looking at runs from somewhere else.

Before concluding that a change is what broke a job, look at the same job on the
run for the parent commit. Some jobs are red for reasons that have nothing to do
with the code, such as toolchain problems on less common platforms or API
permissions on forks, and only the failures that are new relative to the parent
commit are yours.

## Using Docker

In order to get a reproduceable environment for building and testing you can use
[docker](https://www.docker.com). This is also the only faithful way to check a
change to C or C++ code before pushing it: the images build with `-Werror`
settings and compiler versions that a plain local build does not use, so code
that builds cleanly on your machine can still fail the corresponding Github
Actions job. If you are not familiar with how to use it I
would recommend [reading up a bit](https://www.docker.com/get-started) and trying
some simple examples yourself before using it to build and test Erlang/OTP.

There is a pre-built ubuntu base image available on github, but you can also
build it locally if you want to.

Using the pre-built base you build an image like this:

```bash
docker login ghcr.io
git archive --prefix otp/ -o .github/otp.tar.gz HEAD
docker build -t my_otp_image -f .github/dockerfiles/Dockerfile.64-bit .github/
```

This will fetch the ubuntu base image and build a 64-bit Erlang/OTP. You need to
[login to the github container registry](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry)
in order to fetch the base image. If you want to build the base image locally
you can do that like this:

```bash
docker build -t ghcr.io/erlang/otp/ubuntu-base \
  --build-arg BASE=ubuntu:20.04 --build-arg USER=otptest --build-arg uid=$(id -u) \
  --build-arg GROUP=uucp --build-arg gid=$(id -g) \
  -f .github/dockerfiles/Dockerfile.ubuntu-base .github/
```

Which approach is fastest depends on the speed of your internet connection.

When you have built the docker image you can run tests in it like this:

```bash
docker run my_otp_image "make stdlib_test"
```

or if you want to persist the test results outside the container:

```bash
mkdir -m 777 make_test_dir   ## The 777 mode is needed to avoid permission problems
docker run --init -v $PWD/make_test_dir:/buildroot/otp/lib/stdlib/make_test_dir \
  my_otp_image "make stdlib_test"
```

The Common Test logs will be placed in `make_test_dir/ct_logs`.

### Gidpod.io or VSCode dev container

This git repository is also prepared to run using [Gitpod](https://gitpod.io/) or
[VSCode Devcontainer](https://code.visualstudio.com/docs/remote/containers).

The support for these environments is very early so it will be a bit unstable.

To access the gitpod for Erlang/OTP you just navigate to
[https://gitpod.io/#https://github.com/erlang/otp](https://gitpod.io/#https://github.com/erlang/otp).

When using a VSCode dev container, you only need to open [VSCode](https://code.visualstudio.com/)
in the Erlang/OTP repository and you should get a popup that asks if you want to
run in a dev container.

The gitpod and dev container both use the base ubuntu image built in [Using Docker](#using-docker).
So it should be possible to run all tests inside the containers with all test
dependencies available.

*WARNING*: Using VSCode dev container on macOS can be very slow because of limitations
in the filesystem. So I would recommend either using gitpod or just work locally without
the dev container on macOS.
