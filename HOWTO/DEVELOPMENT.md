# Developing Erlang/OTP

The Erlang/OTP development repository is quite large and make system
contains a lot of functionality to help when a developing. This howto
will try to showcase the most important features of the make system.

The make system is not as always as robust as one might like, so if for
any reason something does not work, try doing a `git clean -Xfdq` and
start from the beginning again. This normally only needs to be done when
you jump inbetween different git branches, but it can a good thing to
keep in mind whenever things start to not work as you expect them to.

*WARNING* the functions mentioned in this guide are not supported. This
means that they may be removed or changed without prior notice, so do
not depend on them in CI. For supported make targets see the INSTALL.md
howto.

## Configuring

### Help

The toplevel configure will give help about the features that is provides.
To get a full list of all features you need to use:

```
./configure --help=recur
```

There is documentation for what most of the options mean in the INSTALL.md howto.

### Parellalism

There are multiple configure scripts in this repository and if `MAKEFLAGS`
includes a `-j` flag then they will be run in parallel. So to disable
that parallalism clear `MAKEFLAGS` before running configure:

```
MAKEFLAGS= ./configure
```

## Developing

After you have done configure, you can do `make` on the top of this repository.
That will compile all applications. You can also build a specific application:

```
make stdlib
make common_test
```

These make commands do not manage any dependencies, so if an application needs
something from another you need to make sure that it is built. It is therefore
good practice to first build all applications and then build just the one that
you are updating.

You can also run tests from the top:

```
make test                # Run all tests, takes a long time
make stdlib TYPE=test    # Run only stdlib tests, takes less time
                         # Run only lists_SUITE, takes even less time
make stdlib TYPE=test ARGS="-suite lists_SUIUTE"
```

and you can run static analysis test:

```
make dialyzer            # Checks all of Erlang/OTP source code
make xmllint             # Checks all documentation for xmllint errors
```

Most of the above targets also works for a "phony" target called `emulator` that
represents erts and all its tools. So you can do this:

```
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

```
make TYPE=debug
make FLAVOR=emu
make TYPE=lcnt FLAVOR=emu
```

As you can see it is possible to combine type and flavor to create many different
versions of Erlang. You can then run these different versions by passing the
`-emu_type` and/or `-emu_flavor` flags to `erl`. That is:

```
erl -emu_type lcnt
erl -emu_flavor emu -emu_type debug
```

When running valgrind, asan or gcov 

## lib/APPLICATION

You can also build the application from within itself. Like this:

```
cd lib/stdlib && make
```

Each application has a bunch of make targets that you can use.

```
make                # build all source for this application
make test           # run all tests for this application
make dialyzer       # run dialyze for this application
make doc            # build all docs for this application
make xmllint        # run xmllint on the docs for this application
```


## Static testing

make xmllint
make dialyzer

## Running Test cases
