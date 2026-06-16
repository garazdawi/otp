%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
%%

%% Prototype sketch for IDEAS/07 #15 — Tier 1 of the three-tier
%% prim_tty split.
%%
%% Goal: the minimum prim_tty needs to do at boot to support
%% `io:put_chars/1' to stdout in a non-interactive `-noshell' run.
%%
%% What we DON'T do here:
%%   - tty_create (NIF, allocates a port)
%%   - setlocale (NIF)
%%   - setupterm (NIF, reads $TERM and terminfo DB)
%%   - tcgetattr / tcsetattr (termios)
%%   - reader thread / NIF input port
%%   - ANSI regex compilation
%%
%% What we DO at boot:
%%   - isatty(fd)
%%   - pick UTF-8 vs latin1 based on env (no setlocale NIF)
%%
%% Tier 2 (`prim_tty_writer:capabilities/1') populates the terminfo
%% capability table on first call from `io_ansi' or similar.
%%
%% Tier 3 (`prim_tty:upgrade_from_writer/1') is implemented in the
%% existing `prim_tty' module — it takes the writer state and runs
%% the heavy init for raw mode + reader thread + edlin etc.
%%
%% Triggers (in priority order):
%%   - put_chars                    : Tier 1 only
%%   - io_ansi format               : Tier 2 lazy-populate (no NIF)
%%   - get_line / get_chars / shell : Tier 3 upgrade
%%
%% This module is *not yet wired into user_drv* — the full refactor of
%% `user_drv' to start in `writer' state and lazy-upgrade is the
%% large remaining piece of #15. This file demonstrates the API
%% surface and Tier-1 implementation.

-module(prim_tty_writer).
-moduledoc false.

-export([open/0, open/1, put_chars/2, isatty/1,
         capabilities/1, capabilities_known/1]).

-record(writer, {
          out_fd :: integer(),     %% Output file descriptor (1 for stdout)
          isatty :: boolean(),     %% Result of isatty(out_fd)
          unicode :: boolean(),    %% UTF-8 vs latin1
          caps :: undefined | map() %% Lazy: populated by capabilities/1
         }).

-type state() :: #writer{}.
-export_type([state/0]).

-spec open() -> state().
open() ->
    open(#{}).

-spec open(map()) -> state().
open(Options) ->
    Fd = maps:get(out_fd, Options, 1),
    %% Tier 1: ONE syscall (isatty) + env lookup. No NIF on_load needed.
    IsTty = isatty(Fd),
    %% Encoding decision without setlocale NIF — env vars only.
    Unicode = pick_unicode(IsTty, Options),
    #writer{out_fd = Fd, isatty = IsTty, unicode = Unicode}.

-spec isatty(integer()) -> boolean().
isatty(Fd) ->
    %% In the real implementation this is a tiny NIF (or even a raw
    %% syscall via a port). The existing prim_tty has it as part of
    %% the larger tty_create NIF; we'd factor it out.
    %% Prototype: assume tty if no redirection detected via env.
    case Fd of
        1 -> os:getenv("TERM") =/= false andalso
             os:getenv("STDOUT_REDIRECTED") =:= false;
        2 -> os:getenv("TERM") =/= false andalso
             os:getenv("STDERR_REDIRECTED") =:= false;
        _ -> false
    end.

pick_unicode(_IsTty, #{unicode := Bool}) when is_boolean(Bool) ->
    Bool;
pick_unicode(_IsTty, _) ->
    %% Cheap: scan three env vars for "UTF-8". No setlocale NIF call.
    EnvUTF8 = fun(K) ->
                      case os:getenv(K, "") of
                          "" -> false;
                          V -> string:find(V, "UTF-8") =/= nomatch
                      end
              end,
    lists:any(EnvUTF8, ["LC_ALL", "LC_CTYPE", "LANG"]).

-spec put_chars(state(), unicode:chardata()) -> ok.
put_chars(#writer{out_fd = Fd, unicode = Unicode}, Chars) ->
    %% Encode + write. For the prototype we go through the existing
    %% io infrastructure (which uses the port system) — in the real
    %% implementation a thin write(fd, ...) NIF would be added so the
    %% write path doesn't depend on the file_server / prim_file port
    %% being up either.
    Bin = encode(Chars, Unicode),
    raw_write(Fd, Bin).

encode(Chars, true)  -> unicode:characters_to_binary(Chars, unicode, utf8);
encode(Chars, false) -> unicode:characters_to_binary(Chars, unicode, latin1).

raw_write(Fd, Bin) ->
    %% Prototype: route through erlang:display_string until we add a
    %% tiny write/2 NIF. The point of the API is that the *caller* —
    %% user_drv in writer state — doesn't need anything beyond this.
    case Fd of
        1 -> erlang:display_string(stdout, unicode:characters_to_list(Bin));
        2 -> erlang:display_string(stderr, unicode:characters_to_list(Bin))
    end,
    ok.

%%
%% Tier 2 entry point. Triggered on first call from `io_ansi' or
%% anything that asks for capabilities. Reads $TERM, parses the
%% terminfo file, populates the capability cache. No termios changes,
%% no NIF input port — those are Tier 3.
%%
-spec capabilities(state()) -> {map(), state()}.
capabilities(#writer{caps = Caps} = S) when is_map(Caps) ->
    {Caps, S};
capabilities(#writer{caps = undefined} = S) ->
    Caps = populate_capabilities(S),
    {Caps, S#writer{caps = Caps}}.

-spec capabilities_known(state()) -> boolean().
capabilities_known(#writer{caps = undefined}) -> false;
capabilities_known(#writer{caps = _}) -> true.

populate_capabilities(#writer{isatty = false}) ->
    %% Output is not a tty — pretend it's a dumb terminal with no
    %% colors and no cursor control. io_ansi gates ANSI escapes on
    %% this, so callers get clean output to pipes.
    #{colors => 0, columns => 80, rows => 24, ansi => false};
populate_capabilities(#writer{isatty = true}) ->
    %% Real implementation: call into a small terminfo parser (not the
    %% setupterm NIF — that pulls in the whole terminfo machinery).
    %% For the prototype, infer from $TERM + COLUMNS/LINES env.
    Term = os:getenv("TERM", "dumb"),
    Colors = if Term =:= "dumb"; Term =:= "" -> 0;
                true -> guess_colors(Term)
             end,
    Cols = list_to_integer(os:getenv("COLUMNS", "80")),
    Rows = list_to_integer(os:getenv("LINES", "24")),
    #{colors => Colors, columns => Cols, rows => Rows, ansi => Colors > 0}.

guess_colors(Term) ->
    case string:find(Term, "256color") of
        nomatch ->
            case string:find(Term, "color") of
                nomatch -> 8;
                _ -> 16
            end;
        _ -> 256
    end.
