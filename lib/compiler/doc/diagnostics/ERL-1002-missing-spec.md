# ERL-1002 - Function is missing specification

## Example

```erlang
%% foo.erl
-module(foo).
-export([foo/1]).
foo(0) -> 1.
```

```bash
$ erlc +warn_missing_spec foo.erl
foo.erl:3:1: warning: missing specification for function foo/1 [ERL-1002]
%    3| foo(0) -> 1.
%     | ^
% help: call `erlc -explain ERL-1002` to see a detailed explanation
% note: `warn_missing_spec` was enabled by erlc; 
	use `nowarn_missing_spec` to turn off this warning
```

## Explanation

This warning is disabled by default. It can be enabled by the options
`warn_missing_spec`, `warn_missing_spec_all` and `warn_missing_spec_documented`.
When enabled it triggers a warning for each function that does not have
a function specification.