Testing Erlang/OTP
==================

Before you start testing you need to have the erlang release which you
are going to test in your path. See [$ERL_TOP/HOWTO/INSTALL.md][] for 
instructions on how to build an erlang release.

Short version
-------------

    export ERL_TOP=`pwd`
	./otp_build setup -a
	export PATH=`pwd`/bin:$PATH
	./otp_build tests
	cd release/tests/test_server
	erl -s ts install -s ts run all_tests -s init stop

Where are the tests
-------------------

There are a lot of tests in Erlang/OTP (as of 2012 about 12000) and they are
located in lots of different places throughout the source tree. Below is a list
of the places where you can expect to find tests:

* $ERL_TOP/lib/AppName/test/
* $ERL_TOP/erts/test/
* $ERL_TOP/erts/emulator/test/
* $ERL_TOP/erts/epmd/test/

Some of the tests can be run using [ct_run][] directly at these locations, you
just have to make sure you include the [ct hook][] called ts_install_cth in your
path. Tests which require C files to be built *cannot* be tested using this
method.


Writing tests
-------------

All tests are [common_test][] suites and follow the same pattern as all 
[common_test][] suites. There are however a couple of corner cases which are
handled by a test wrapper called `ts`. `ts` allows the test writer to put
`Makefile.src` and `Makefile.first` files in the [data_dir][] and a special
directory called `all_SUITE_data`. 

`Makefile.first` is run before any other Makefile and is typically used to
generate .hrl files which are needed by other test suites. As of 2012 only
the erl_interface tests use this feature. 

`Makefile.src` is configured to a `Makefile` using the `variables` created when
[configuring the tests][]. These `Makefile`s are later run by the test suite
to compile whatever platform specific code which the tests need to run.

Releasing tests
---------------

If you cannot use [ct_run][] in the source tree you have to release the tests
into a common test directory. The easiest way to do this is to use `otp_build`
like this:

    ERL_TOP=`pwd`; ./otp_build tests
	
This will release all tests in Erlang/OTP to `$ERL_TOP/release/tests/`. If you 
want to change the directory where the tests are released to use the `TESTROOT`
environmantal variable. 

In the `$TESTROOT` you should now see lots of folders called *_test, these 
folders contain everything needed to test Erlang/OTP and are platform 
independent, so if you are testing Erlang on multiple platforms you just have
to release on one and copy them to all others.

### Releasing cross tests

For releasing tests in a cross compile environment see [$ERL_TOP/HOWTO/INSTALL-CROSS.md][].

Configuring and Running tests
-----------------------------

Running tests is done by first navigating to the `$TESTROOT/test_server` folder
created when you released the tests and then start `erl` in that directory. The
setting which you use to start the emulator will be carried with you into the
tests, so if you want to test using async threads you have to supply `+A 10` to
`erl` when you start it. 

To configure and run the tests `ts` is used. `ts` is a wrapped to
[common_test][] which takes care of configuration and build issues before 
[common_test][] is started.

### Configuring the test environment

Before running released tests you have to install them on the target system.
Installing the tests is done by calling `ts:install().` in the erlang shell
which you intend to test from. `ts:install()` is basically a wrapped to a 
configure script and some erlang code which figures out what your system looks
like and what kind of emulator you are testing with. `ts:install()` can also 
take some arguments if neccesary, see `ts:help()` for details. 

All variables created by `ts:install()` are found in 
`$TESTROOT/test_server/variables`. 

### Running the tests

To run all test suites do

    ts:run().

Note that running all tests will require several hours, so you may want to run 
the test cases for a single application

    ts:run(Application, [batch]).

or even part of the test suite for an application, for example

    ts:run(emulator, bs, [batch]).

to run all test suite modules starting with `bs` (i.e. all modules that test 
the bit syntax).

To run a specific test case in a module, the full name of the module and test 
case must be spelled out:

    ts:run(emulator, bs_bincomp_SUITE, byte_aligned, [batch]).

Run `ts:help().` to show some help.

As of R14B02 it is also possibly to start all tests but the erl_interface tests 
by invoking Common Test directly from the released applications test directory, 
i.e.

    cd release/tests/test_server
    $ERL_TOP/bin/ct_run -suite ../compiler_test/andor_SUITE -case t_orelse

Running [ct_run][] from the command line still requires you to do the 
`ts:install()` step above.

Examining the results
---------------------

Open the file `release/tests/test_server/index.html` in a web browser. Or open 
`release/tests/test_server/last_test.html` when a test suite is running to 
examine the results so far for the currently executing test suite (in and later 
R14B02 you want to open the `release/tests/test_server/all_runs.html` file to 
get to the currently running test)

   [ct_run]: http://www.erlang.org/doc/man/ct_run.html
   [ct hook]: http://www.erlang.org/doc/apps/common_test/ct_hooks_chapter.html
   [$ERL_TOP/HOWTO/INSTALL.md]: INSTALL.md
   [$ERL_TOP/HOWTO/INSTALL-CROSS.md]: INSTALL-CROSS.md#testing-the-cross-compiled-system
   [common_test]: http://www.erlang.org/doc/man/ct.html
   [data_dir]: http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#data_priv_dir
   [configuring the tests]: #configuring-the-test-environment

   [?TOC]: true
