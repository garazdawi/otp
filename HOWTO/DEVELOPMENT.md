# Developing Erlang/OTP

The Erlang/OTP development repository is quite large and make system
contains a lot of functionality to help when a developing. This howto
will try to showcase the most important features of the make system.

The guide is mostly aimed towards development on a Unix platform, but
most things should work also work on Windows.

*WARNING* the functions mentioned in this guide are not supported. This
means that they may be removed or changed without prior notice, so do
not depend on them in CI. For supported make targets see the
[Install howto](INSTALL.md) and the [Testing howto](TESTING.md).

The make system is not as always as robust as one might like, so if for
any reason something does not work, try doing a `git clean -Xfdq` and
start from the beginning again. This normally only needs to be done when
you jump inbetween different git branches, but it can a good thing to
keep in mind whenever things start to not work as you expect them to.

## Short version

First do this:

```bash
git clone -b maint git@github.com:erlang/otp
cd otp && export ERL_TOP=`pwd`
./configure && make
```

After making changes, adding tests and updating documentation. You do
this to make and test the changes:

```bash
cd lib/$APPLICATION_NAME
make            # Rebuid application
make test       # Run application tests
make dialyzer   # Run dialyzer
make docs       # Build the docs
make xmllint    # Run xmllint on the docs
```

## Preparations

Before you start working you need to clone the Erlang/OTP git repository
and install any dependencies that you do not have. See
[Required Utilities](INSTALL.md#required-utilities) and
[Optional Utilities](INSTALL.md#optional-utilities) in [INSTALL.md](INSTALL.md)
for a list of utilities to install.

Make sure that you have read the [Contributing to Erlang/OTP](../CONTRIBUTING.md)
guide if you intend to make a contribution to Erlang/OTP.

### Faster builds

Both `./configure` and `make` take advantage of running in parallel if told to,
so in order to speed up your development environment make sure to set:

```bash
export MAKEFLAGS=-jN        ## Change N to be the number of CPUs available
```

The Erlang compiler can be run using a [Compile Server](https://www.erlang.org/doc/man/erlc.html#compile-server),
this normally cuts a minute or two from the total build time of Erlang/OTP.
To enable set this environment variable:

```bash
export ERLC_USE_SERVER=true
```

## Configuring

You run configure by issuing the command:

```bash
./configure
```

If you change any `Makefile`s you will need to re-run configure.
If you update any configure.ac scripts you need to
[update the configure scripts](INSTALL.md#updating-configure-scripts).

### Help

The toplevel configure will give help about the features that is provides.
To get a full list of all features you need to use:

```bash
./configure --help=recur
```

There is documentation for what most of the options mean in the
[INSTALL.md howto](INSTALL.md#Configuring).

## Developing

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
make test                # Run all tests, takes a long time
make stdlib TYPE=test    # Run only stdlib tests, takes less time
                         # Run only lists_SUITE, takes even less time
make stdlib TYPE=test ARGS="-suite lists_SUITE"
```

and you can run static analysis test:

```bash
make dialyzer            # Checks all of Erlang/OTP source code
make xmllint             # Checks all documentation for xmllint errors
```

Most of the above targets also works for a "phony" target called `emulator` that
represents erts and all its tools. So you can do this:

```bash
make emulator            # Build erts, epmd etc
make emulator TYPE=test  # Run all emulator tests
```

### Types and Flavors

Erlang can be built using different types and flavors. Mostly the types and
flavors change how the Erlang VM itself is built, but some also effect how
application are built. Some of the types/flavors are:

* types
  * opt (default)
  * debug
  * lcnt
  * valgrind
  * asan
  * gcov
* flavor
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

## lib/APPLICATION

You can also build the application from within itself. Like this:

```bash
cd lib/stdlib && make
```

Each application has a bunch of make targets that you can use.

```bash
make                # build all source for this application
make test           # run all tests for this application
make dialyzer       # run dialyze for this application
make doc            # build all docs for this application
make xmllint        # run xmllint on the docs for this application
```

## cerl

`cerl` is a program available in `$ERL_TOP/bin/` that has a number of features
useful when developing the Erlang run-time system. It work just as normal `erl`,
but accepts a couple of extra command line switches. Any other command line arguments
passed to `cerl` will be passed on the Erlang as normal. The extra command line
switches are:

* -debug
  * Start a debug run-time system
* -lcnt
  * Start a lock count run-time system
* -valgrind
  * Start valgrind with the correct settings and use the `valgrind` [type](types-and-flavors).
  * Set environment variable `VALGRIND_LOG_XML` to true if want xml valgrind logs.
  * Set environment variable `VALGRIND_LOG_DIR` to where you want valgrind logs.
  * Set environment variable `VALGRIND_MISC_FLAGS` for any extra valgrind flags you want to pass.
* -asan
  * Start asan with the the correct settings and use the `asan` [type](types-and-flavors).
  * Set environment variable `ASAN_LOG_DIR` to where you want the logs.
  * Set environment variable `ASAN_OPTIONS` for any extra asan options you want to pass.
* -gcov
  * Start a gcov run-time system
* -gdb
  * Start an Emacs gdb debugging session
* -core /path/to/core/file
  * Start an Emacs gdb debugging session for the core specified
* -rgdb
  * Start a gdb debugging session in the current terminal
* -rcore /path/to/core/file
  * Start a gdb debugging session in the current terminal for the core specified
* -lldb
  * Start a lldb debugging session in the current terminal
* -rr
  * Start Erlang under [rr](https://rr-project.org/) to record all events
* -rr replay [session]
  * Load a recording session using `rr replay`, if no session is specified the latest run session is laoded.

## Static testing

make xmllint
make dialyzer

## Running Test cases
