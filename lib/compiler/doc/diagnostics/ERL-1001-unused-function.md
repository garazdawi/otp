# ERL-1001 - Unused functions

## Example

```erlang
%% foo.erl
-module(foo).
-export([foo/1]).
foo(0) -> 1.
boo(1) -> 2.
```

```bash
$ erlc foo.erl
foo.erl:4:1: warning: function bar/1 is unused [ERL-1001]
%    4| bar(1) -> 2.
%     | ^
% help: call `erlc -explain ERL-1001` to see a detailed explanation
% note: `warn_unused_function` was enabled by default; 
        use `nowarn_unused_function` to turn off this warning
```

## Explanation

This warning is emitted when when a function in a module is not exported and
not used by any other function within the same module.