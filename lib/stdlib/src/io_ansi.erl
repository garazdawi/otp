%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

%% 
%% https://hexdocs.pm/elixir/IO.ANSI.html
%% https://en.wikipedia.org/wiki/ANSI_escape_code
%% https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
%% https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
%%

-module(io_ansi).
-moduledoc """
Controlling the terminal using virtual terminal sequences (aka [ANSI escape codes]).

This module provides an interface to emit and parse virtual terminal sequences (VTS),
also known as [ANSI escape codes]. VTS can be used to:

- change the style of text or background in the terminal by adding color or emphasis.
- delete printed characters or lines.
- move, hide or show the cursor

and more things. As different terminals are interpret VTSs slightly
differently, `m:io_ansi` uses the local [terminfo] database together with
predefined sequences to emit the correct sequence for the terminal that is
currently used. To fetch values directly from the [terminfo] database you can use
`tput/2`, `tigetnum/1` and `tigetflag/1`.

`m:io_ansi` provides two interfaces to emit sequences. You can either call the
function representing the sequence you want to emit, for example `io_ansi:blue()`
and it will return the sequence representing blue.

```erlang
1> io_ansi:blue().
<<"\e[34m">>
```

This will use the [terminfo] database locally where the call is made, so it may
not be correct if used across nodes.

You can also use the [`io_ansi:format/1,2,3`](`io_ansi:format/3`) functions
which works just as `io_lib:bformat/3`, except that it also accepts atoms and
tuples that represent VTSs. For example:

```erlang
1> io_ansi:format([blue,"~p"], [red]).
<<"\e[34mred\e(B\e[m">>
```

`io_ansi:format/3` will automatically reset the terminal to its original state
and strip any VTSs that are not supported by the terminal. It can also be disabled
through an option. For example:

```erlang
1> io_ansi:format([blue,"~p"], [red], [{enabled, false}]).
<<"red">>
```

Finally there is [`io_ansi:fwrite/1,2,3,4`](`io_ansi:fwrite/4`) which does not
return the string to be printed, but instead sends it to the `t:io:device/0`
that should handle it. `io_ansi:fwrite/4` works across nodes and will use the
[terminfo] database where the data is outputted to decide what to emit.

[terminfo]: https://man7.org/linux/man-pages/man5/terminfo.5.html
[ANSI escape codes]: https://en.wikipedia.org/wiki/ANSI_escape_code
""".


-export([tput/1, tput/2, tigetnum/1, tigetflag/1, tinfo/0]).
-export([format/1, format/2, format/3, fwrite/1, fwrite/2, fwrite/3, fwrite/4,
         enabled/0, enabled/1, scan/1]).

-export([black/0, blue/0, cyan/0, green/0, magenta/0, red/0, white/0, yellow/0,
         color/1, color/3, default_color/0]).
-export([black_background/0, red_background/0, green_background/0, yellow_background/0,
         blue_background/0, magenta_background/0, cyan_background/0, white_background/0,
         background/1, background/3, default_background/0]).
-export([light_black/0, light_red/0, light_green/0, light_yellow/0, light_blue/0,
         light_magenta/0, light_cyan/0, light_white/0]).
-export([light_black_background/0, light_red_background/0, light_green_background/0,
         light_yellow_background/0, light_blue_background/0, light_magenta_background/0,
         light_cyan_background/0, light_white_background/0]).
-export([modify_color/4]).
-export([bold/0, bold_off/0, underline/0, underline_off/0, negative/0, negative_off/0]).
-export([hyperlink_start/1, hyperlink_start/2, hyperlink_reset/0]).
-export([clear/0, erase_display/0, insert_character/1, delete_character/0,
         delete_character/1, erase_character/1, insert_line/0, insert_line/1,
         delete_line/0, delete_line/1, erase_line/0]).
-export([alternate_character_set_mode/0, alternate_character_set_mode_off/0]).
-export([cursor/2, cursor_up/0, cursor_up/1, cursor_down/0, cursor_down/1,
         cursor_forward/0, cursor_forward/1, cursor_backward/0, cursor_backward/1,
         cursor_home/0, reverse_index/0, cursor_save/0, cursor_restore/0, 
         cursor_show/0, cursor_hide/0, 
         cursor_next_line/0, cursor_previous_line/0, cursor_horizontal_absolute/1,
         cursor_vertical_absolute/1, cursor_horizontal_vertical/2, cursor_report_position/0]).
-export([alternate_screen/0, alternate_screen_off/0,
        scroll_forward/0, scroll_forward/1, scroll_backward/0, scroll_backward/1,
        scroll_change_region/2]).
-export([tab/0, tab_backward/0, tab_set/0, tab_clear/0, tab_clear_all/0]).
-export([keypad_transmit_mode/0, keypad_transmit_mode_off/0]).
-export([reset/0, device_report_attributes/0]).

-doc "The format string that can be passed to `format/3` and `fwrite/4`".
-type format() :: [string() | code()].

-doc "Virtual terminal codes that control the foreground (aka text) color.".
-type foreground_color() :: black | blue | cyan | green | magenta | red | white | yellow |
                            light_black | light_blue | light_cyan | light_green |
                            light_magenta | light_red | light_white | light_yellow |
                            {color, 0..255} | {color, R :: 0..255, G :: 0..255, B :: 0..255} |
                            default_color.
-doc "Virtual terminal codes that control the background color.".
-type background_color() :: black_background | blue_background | cyan_background |
                            green_background | magenta_background | red_background |
                            white_background | yellow_background | default_background |
                            light_black_background | light_blue_background |
                            light_cyan_background | light_green_background |
                            light_magenta_background | light_red_background |
                            light_white_background | light_yellow_background |
                            {color_background, 0..255} |
                            {color_background, R :: 0..255, G :: 0..255, B :: 0..255}.
-doc "Virtual terminal codes that control color.".
-type color() :: foreground_color() | background_color() |
                 {modify_color, Index :: 0..255, R :: 0..255, G :: 0..255, B :: 0..255}.

-doc "Virtual terminal codes that control text style.".
-type style() :: bold | bold_off | underline | underline_off | negative | negative_off.

-type hyperlink_params() :: [{Key :: unicode:chardata(), Value :: unicode:chardata()}].

-doc "Virtual terminal codes that control whether emitted text shall be a hyper link or not.".
-type hyperlink() :: {hyperlink, URL :: uri_string:uri_string(), Text :: unicode:chardata()} |
                     {hyperlink, URL :: uri_string:uri_string(), hyperlink_params(), Text :: unicode:chardata()} |
                     {hyperlink_start, URL :: uri_string:uri_string()} |
                     {hyperlink_start, URL :: uri_string:uri_string(), hyperlink_params()} |
                     hyperlink_reset.

-doc "Virtual terminal codes that control text formatting.".
-type text_formatting() :: color() | style() | hyperlink().
-doc "Virtual terminal codes that can erase or owerwrite text.".
-type text_modification() :: clear | erase_display |
                             insert_character | delete_character | erase_character |
                             insert_line | delete_line | erase_line.
-doc "Virtual terminal codes that works on text.".
-type text() :: text_formatting() | text_modification() |
                alternate_character_set_mode | alternate_character_set_mode_off.
-doc "Virtual terminal codes that controls the cursor.".
-type cursor() ::
        {cursor, Line :: non_neg_integer(), Column :: non_neg_integer()} |
        cursor_down | cursor_up | cursor_backward | cursor_forward |
        {cursor_down | cursor_backward | cursor_forward | cursor_up, N :: non_neg_integer()} |
        cursor_home | reverse_index | cursor_save | cursor_restore |
        cursor_show | cursor_hide |
        cursor_next_line | cursor_previous_line | cursor_horizontal_absolute |
        cursor_vertical_absolute | cursor_horizontal_vertical | cursor_report_position.
-doc "Virtual terminal codes that controls the screen.".
-type window() :: alternate_screen | alternate_screen_off |
                  scroll_forward | scroll_backward | scroll_change_region.
-doc "Virtual terminal codes that works with tabs.".
-type tab() :: tab | tab_backward | tab_set | tab_clear | tab_clear_all.
-doc "Virtual terminal codes for cursor input.".
-type input() :: keypad_transmit_mode | keypad_transmit_mode_off |
        kcursor_down | kcursor_up | kcursor_backward | kcursor_forward | 
        kcursor_home | kcursor_end.
-doc "Virtual terminal codes.".
-type code() :: text() | cursor() | window() | tab() | input() | reset | device_report_attributes.

-type option() :: {reset, boolean()} | { enabled, boolean()} | io_lib:format_options().
-type options() :: [option()].

-export_type([code/0]).

-doc #{ equiv => tput(TermInfoCap, []) }.
-doc #{ group => ~"Functions: terminfo" }.
-spec tput(TermInfoCap :: string()) -> unicode:unicode_binary().
tput(TermInfoCap) ->
    tput(TermInfoCap, []).

-doc """
Returns the string representing the action taken by the given terminal capability.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tput/2` will use the terminfo definition
associated with the `TERM` environment variable when the Erlang VM is started.
It is not possible to change after startup.

If the given capability is not defined in the terminfo database an `enotsup`
error is generated, if the given capability is invalid a `badarg` error is 
generated.

This function does not work on Windows and will always generate a `badarg`
exception.

Example:

```
%% Set the foreground color to 3
1> io_ansi:tput("setaf",[3]).
<<"\e[33m">>
%% Move the cursor up 2 spaces
2> io_ansi:tput("cuu",[2]).
<<"\e[2A">>
%% Move the cursor down 1 space
3> io_ansi:tput("cud1").
<<"\n">>
%% unsupported capability
4> io_ansi:tput("slm").
** exception error: {enotsup,"slm"}
     in function  io_ansi:tput/2
%% unknown capability
5> io_ansi:tput("foobar").
** exception error: {einval,"foobar",[]}
     in function  io_ansi:tput/2
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tput(TermInfoCapName :: string(), Args :: [integer()]) ->
    unicode:unicode_binary().
tput(TermInfoCap, Args) ->
    try prim_tty:tigetstr(TermInfoCap) of
        {ok, TermInfoStr} ->
            try prim_tty:tputs(TermInfoStr, Args) of
                {ok, S} -> S
            catch error:badarg ->
                erlang:error({badarg, TermInfoCap, Args})
            end;
        false ->
            erlang:error({enotsup, TermInfoCap})
    catch error:badarg ->
        erlang:error({einval, TermInfoCap, Args})
    end.

-doc """
Returns the number representing a terminfo capability.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tigetnum/1` will use the terminfo
definition associated with the `TERM` environment variable when the Erlang VM is
started. It is not possible to change after startup.

Returns `-1` if the capability is not available.

Example:

```erlang
1> io_ansi:tigetnum("co").
80
2> io_ansi:tigetnum("foobar").
-1
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tigetnum(TermInfoCapName :: string()) -> -1 | non_neg_integer().
tigetnum(TermInfoCap) ->
    prim_tty:tigetnum(TermInfoCap).

-doc """
Returns the true if the terminfo capability is available, otherwise false.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tigetflag/1` will use the terminfo
definition associated with the `TERM` environment variable when the Erlang VM is
started. It is not possible to change after startup.

Example:

```erlang
1> io_ansi:tigetflag("xn").
true
2> io_ansi:tigetflag("foobar").
false
```
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tigetflag(TermInfoCapName :: string()) -> boolean().
tigetflag(TermInfoCap) ->
    prim_tty:tigetflag(TermInfoCap).

-doc """
Returns information about all available terminfo capabilities. See
the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation for details on each.

`tinfo/0` will use the terminfo definition associated with the `TERM` environment
variable when the Erlang VM is started. It is not possible to change after startup.

When calling `tput/2`, `tigetnum/1` and `tigetflag/1` you should provide the `name`
of the capability you want.
""".
-doc #{ group => ~"Functions: terminfo" }.
-spec tinfo() -> #{ bool := [#{ code := string(), name := string(), full_name := string()}]}.
tinfo() -> 
    prim_tty:tinfo().

-define(FUNCTION(NAME),
        NAME() -> Fun = lookup(NAME, 0), Fun()).
-define(FUNCTION(NAME, ARG1),
        NAME(ARG1) -> Fun = lookup(NAME, 1), Fun(ARG1)).
-define(FUNCTION(NAME, ARG1, ARG2),
        NAME(ARG1, ARG2) -> Fun = lookup(NAME, 2), Fun(ARG1, ARG2)).
-define(FUNCTION(NAME, ARG1, ARG2, ARG3),
        NAME(ARG1, ARG2, ARG3) -> Fun = lookup(NAME, 3), Fun(ARG1, ARG2, ARG3)).
-define(FUNCTION(NAME, ARG1, ARG2, ARG3, ARG4),
        NAME(ARG1, ARG2, ARG3, ARG4) -> Fun = lookup(NAME, 4), Fun(ARG1, ARG2, ARG3, ARG4)).

-define(SPEC(NAME),
        -spec NAME() -> unicode:chardata()).
-define(SPEC(NAME, ARG1),
        -spec NAME(ARG1 :: integer()) -> unicode:chardata()).
-define(SPEC(NAME, ARG1, ARG2),
        -spec NAME(ARG1 :: integer(), ARG2 :: integer()) -> unicode:chardata()).

-doc """
Change foreground (aka text) color to black.
""".
?SPEC(black).
?FUNCTION(black).
-doc """
Change foreground (aka text) color to red.
""".
?SPEC(red).
?FUNCTION(red).
-doc """
Change foreground (aka text) color to gren.
""".
?SPEC(green).
?FUNCTION(green).
-doc """
Change foreground (aka text) color to yellow.
""".
?SPEC(yellow).
?FUNCTION(yellow).
-doc """
Change foreground (aka text) color to blue.
""".
?SPEC(blue).
?FUNCTION(blue).
-doc """
Change foreground (aka text) color to magenta.
""".
?SPEC(magenta).
?FUNCTION(magenta).
-doc """
Change foreground (aka text) color to cyan.
""".
?SPEC(cyan).
?FUNCTION(cyan).
-doc """
Change foreground (aka text) color to white.
""".
?SPEC(white).
?FUNCTION(white).
-doc """
Change foreground (aka text) color to index color. `Index` 0-15 are equivilant to
the named colors in `t:foreground_color/0` in the order that they are listed.
""".
-spec color(Index :: 0..255 | 0..87) -> unicode:chardata().
?FUNCTION(color, Index).
-doc """
Change foreground (aka text) color to RGB color.
""".
-spec color(0..255, 0..255, 0..255) -> unicode:chardata().
?FUNCTION(color, Red, Green, Blue).
-doc """
Change foreground (aka text) color to the default color.
""".
?SPEC(default_color).
?FUNCTION(default_color).

-doc """
Change background color to black.
""".
?SPEC(black_background).
?FUNCTION(black_background).
-doc """
Change background color to ref.
""".
?SPEC(red_background).
?FUNCTION(red_background).
-doc """
Change background color to green.
""".
?SPEC(green_background).
?FUNCTION(green_background).
-doc """
Change background color to yellow.
""".
?SPEC(yellow_background).
?FUNCTION(yellow_background).
-doc """
Change background color to blue.
""".
?SPEC(blue_background).
?FUNCTION(blue_background).
-doc """
Change background color to magenta.
""".
?SPEC(magenta_background).
?FUNCTION(magenta_background).
-doc """
Change background color to cyan.
""".
?SPEC(cyan_background).
?FUNCTION(cyan_background).
-doc """
Change background color to white.
""".
?SPEC(white_background).
?FUNCTION(white_background).
-doc """
Change background color to index color. `Index` 0-15 are equivilant to
the named colors in `t:background_color/0` in the order that they are listed.
""".
-spec background(Index :: 0..255 | 0..87) -> unicode:chardata().
?FUNCTION(background, Index).
-doc """
Change background color to RGB color.
""".
-spec background(0..255, 0..255, 0..255) -> unicode:chardata().
?FUNCTION(background, Red, Green, Blue).
-doc """
Change background color to the default color.
""".
?SPEC(default_background).
?FUNCTION(default_background).

-doc """
Change foreground (aka text) color to light black.
""".
?SPEC(light_black).
?FUNCTION(light_black).
-doc """
Change foreground (aka text) color to light red.
""".
?SPEC(light_red).
?FUNCTION(light_red).
-doc """
Change foreground (aka text) color to light green.
""".
?SPEC(light_green).
?FUNCTION(light_green).
-doc """
Change foreground (aka text) color to light yellow.
""".
?SPEC(light_yellow).
?FUNCTION(light_yellow).
-doc """
Change foreground (aka text) color to light magenta.
""".
?SPEC(light_magenta).
?FUNCTION(light_magenta).
-doc """
Change foreground (aka text) color to light blue.
""".
?SPEC(light_blue).
?FUNCTION(light_blue).
-doc """
Change foreground (aka text) color to light cyan.
""".
?SPEC(light_cyan).
?FUNCTION(light_cyan).
-doc """
Change foreground (aka text) color to light white.
""".
?SPEC(light_white).
?FUNCTION(light_white).

-doc """
Change background color to light black.
""".
?SPEC(light_black_background).
?FUNCTION(light_black_background).
-doc """
Change background color to light red.
""".
?SPEC(light_red_background).
?FUNCTION(light_red_background).
-doc """
Change background color to light green.
""".
?SPEC(light_green_background).
?FUNCTION(light_green_background).
-doc """
Change background color to light yellow.
""".
?SPEC(light_yellow_background).
?FUNCTION(light_yellow_background).
-doc """
Change background color to light magenta.
""".
?SPEC(light_magenta_background).
?FUNCTION(light_magenta_background).
-doc """
Change background color to light blue.
""".
?SPEC(light_blue_background).
?FUNCTION(light_blue_background).
-doc """
Change background color to light cyan.
""".
?SPEC(light_cyan_background).
?FUNCTION(light_cyan_background).
-doc """
Change background color to light white.
""".
?SPEC(light_white_background).
?FUNCTION(light_white_background).

-doc """
Modify the color referenced by `Index` to be RGB.

Calling this function for `Index` 0-15 will change the color of the named colors
in `t:foreground_color/0` and `t:background_color/0`.
""".
-spec modify_color(Index :: 0..255, R :: 0..255, G :: 0..255, B :: 0..255) -> unicode:chardata().
?FUNCTION(modify_color, Index, R, G, B).

-doc """
Turn on bold text style.
""".
?SPEC(bold).
?FUNCTION(bold).
-doc """
Turn off bold text style.
""".
?SPEC(bold_off).
?FUNCTION(bold_off).
-doc """
Turn on underline text style.
""".
?SPEC(underline).
?FUNCTION(underline).
-doc """
Turn off underline text style.
""".
?SPEC(underline_off).
?FUNCTION(underline_off).
-doc """
Turn on negative text style.
""".
?SPEC(negative).
?FUNCTION(negative).
-doc """
Turn off negative text style.
""".
?SPEC(negative_off).
?FUNCTION(negative_off).


-doc """
Clear screen and set cursor to home.
""".
?SPEC(clear).
?FUNCTION(clear).
-doc """
Clear screen after cursor.
""".
?SPEC(erase_display).
?FUNCTION(erase_display).
-doc """
Insert `Chars` at cursor.
""".
?SPEC(insert_character, Chars).
?FUNCTION(insert_character, Chars).
-doc """
Delete 1 character at cursor.
""".
?SPEC(delete_character).
?FUNCTION(delete_character).
-doc """
Delete `Chars` characters at cursor by shifting the text `Chars` characters to
the left.
""".
?SPEC(delete_character, Chars).
?FUNCTION(delete_character, Chars).
-doc """
Erase `Chars` characters at cursor by making `Chars` characters before the
cursor blank.
""".
?SPEC(erase_character, Chars).
?FUNCTION(erase_character, Chars).
?SPEC(insert_line).
?FUNCTION(insert_line).
?SPEC(insert_line, Lines).
?FUNCTION(insert_line, Lines).
?SPEC(delete_line).
?FUNCTION(delete_line).
?SPEC(delete_line, Lines).
?FUNCTION(delete_line, Lines).
?SPEC(erase_line).
?FUNCTION(erase_line).

?SPEC(alternate_character_set_mode).
?FUNCTION(alternate_character_set_mode).
?SPEC(alternate_character_set_mode_off).
?FUNCTION(alternate_character_set_mode_off).

-doc """
Move the cursor to the given position. Position 0,0 is at the top left of the
terminal.
""".
?SPEC(cursor, Line, Column).
?FUNCTION(cursor, Line, Column).

?SPEC(cursor_up).
?FUNCTION(cursor_up).
?SPEC(cursor_up, Lines).
?FUNCTION(cursor_up, Lines).

?SPEC(cursor_down).
?FUNCTION(cursor_down).
?SPEC(cursor_down, Lines).
?FUNCTION(cursor_down, Lines).

?SPEC(cursor_forward).
?FUNCTION(cursor_forward).
?SPEC(cursor_forward, Lines).
?FUNCTION(cursor_forward, Lines).

?SPEC(cursor_backward).
?FUNCTION(cursor_backward).
?SPEC(cursor_backward, Lines).
?FUNCTION(cursor_backward, Lines).

?SPEC(cursor_home).
?FUNCTION(cursor_home).

?SPEC(reverse_index).
?FUNCTION(reverse_index).
?SPEC(cursor_save).
?FUNCTION(cursor_save).
?SPEC(cursor_restore).
?FUNCTION(cursor_restore).

?SPEC(cursor_show).
?FUNCTION(cursor_show).
?SPEC(cursor_hide).
?FUNCTION(cursor_hide).

?SPEC(cursor_next_line).
?FUNCTION(cursor_next_line).
?SPEC(cursor_previous_line).
?FUNCTION(cursor_previous_line).
?SPEC(cursor_horizontal_absolute, X).
?FUNCTION(cursor_horizontal_absolute, X).
?SPEC(cursor_vertical_absolute, Y).
?FUNCTION(cursor_vertical_absolute, Y).
?SPEC(cursor_horizontal_vertical, X, Y).
?FUNCTION(cursor_horizontal_vertical, X, Y).
?SPEC(cursor_report_position).
?FUNCTION(cursor_report_position).

?SPEC(alternate_screen).
?FUNCTION(alternate_screen).
?SPEC(alternate_screen_off).
?FUNCTION(alternate_screen_off).

?SPEC(scroll_forward).
?FUNCTION(scroll_forward).
?SPEC(scroll_forward, Steps).
?FUNCTION(scroll_forward, Steps).
?SPEC(scroll_backward).
?FUNCTION(scroll_backward).
?SPEC(scroll_backward, Steps).
?FUNCTION(scroll_backward, Steps).
?SPEC(scroll_change_region, Line1, Line2).
?FUNCTION(scroll_change_region, Line1, Line2).

?SPEC(tab).
?FUNCTION(tab).
?SPEC(tab_backward).
?FUNCTION(tab_backward).
?SPEC(tab_set).
?FUNCTION(tab_set).
?SPEC(tab_clear).
?FUNCTION(tab_clear).
?SPEC(tab_clear_all).
?FUNCTION(tab_clear_all).

?SPEC(keypad_transmit_mode).
?FUNCTION(keypad_transmit_mode).
?SPEC(keypad_transmit_mode_off).
?FUNCTION(keypad_transmit_mode_off).

?SPEC(reset).
?FUNCTION(reset).

?SPEC(device_report_attributes).
?FUNCTION(device_report_attributes).

%% See https://gcc.gnu.org/cgit/gcc/commit/?id=458c8d6459c4005fc9886b6e25d168a6535ac415 for
%% details on how to check whether we can use terminal URLs.
%% The specification for how to ANSI URLs work is here: https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
-spec hyperlink_start(uri_string:uri_string()) -> unicode:chardata().
?FUNCTION(hyperlink_start, URL).
-spec hyperlink_start(uri_string:uri_string(),
        [{Key :: unicode:chardata(), Value :: unicode:chardata()}]) -> unicode:chardata().
?FUNCTION(hyperlink_start, URL, Params).
?SPEC(hyperlink_reset).
?FUNCTION(hyperlink_reset).

-doc """
Check if `t:io:user/0` can interpret ANSI escape sequences.
""".
-spec enabled() -> boolean().
enabled() ->
    enabled(user).

-doc """
Check if `Device` can interpret ANSI escape sequences.

This is done by checking if `Device` represents a terminal and if the `TERM`
environment variable is set to a terminal type that supports virtual terminal
sequences.
""".
-spec enabled(io:device()) -> boolean().
enabled(Device) ->
    IsTerminal =
        case io:getopts(Device) of
            {error, _} -> false;
            Opts ->
                proplists:get_value(terminal, Opts, false)
        end,
    IsSmartTerminal =
        case os:type() of
            {win32, _} -> true;
            _ ->
                prim_tty:tigetstr("sgr0") =/= false
        end,
    IsTerminal andalso IsSmartTerminal.

-doc """
Scan the string for virtial terminal sequences.

The recognized VTSs will be converted into the corresponding `t:code/0`.

If you intend to parse arrow keys it is recommended that you first set the terminal in
application mode by using `io_ansi:format(standard_out, [keypad_transmit_mode], [], [])`.
This will make it easier for io_ansi to correctly detect arrow keys.

Any unrecognized [control sequence introducers](https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands),
will be placed in a tuple tagged with `csi`.

Example:

```erlang
1> io_ansi:scan("\eOA").
[kcursor_up]
2> io_ansi:scan("\eOB").
[kcursor_down]
3> io_ansi:scan(io_ansi:format([bold, "text"])).
[bold, ~"text", reset]
4> io_ansi:scan(io_ansi:format([{cursor, 0, 0}])).
[{csi, ~"\e[1;1H"}, reset]
```

""".
-spec scan(unicode:chardata()) -> [unicode:unicode_binary() | code() | {csi, unicode:unicode_binary()}].
scan(Data) ->
    scan_binary(unicode:characters_to_binary(Data), <<>>, []).

scan_binary(<<CSI/utf8, R/binary>> = Data, Bin, Acc) when CSI =:= $\e; CSI =:= 155 ->
    case lookup_vts(Data) of
        undefined ->
            case re:run(Data, prim_tty:ansi_regexp(), [unicode]) of
                {match, [{0, N}]} ->
                    <<Ansi:N/binary, AnsiRest/binary>> = Data,
                    scan_binary(AnsiRest, <<>>, [{csi, Ansi}, Bin | Acc]);
                nomatch ->
                        scan_binary(R, <<Bin/binary, CSI/utf8>>, Acc)
            end;
        {Code, _, Rest} ->
            scan_binary(Rest, <<>>, [Code, Bin | Acc])
    end;
scan_binary(<<C/utf8, R/binary>>, Bin, Acc) ->
    scan_binary(R, <<Bin/binary, C/utf8>>, Acc);
scan_binary(<<>>, Bin, Acc) ->
    [NonEmpty || NonEmpty <- lists:reverse([Bin | Acc]), NonEmpty =/= <<>>]. 

lookup_vts(Data) ->
    try
        [case Data of
                <<Value:(byte_size(Value))/binary, Rest/binary>> ->
                    throw({Key, Value, Rest});
                _ ->
                    ok
            end || Key := Values <- get_vts_mappings(),
             Value <- Values],
        undefined
    catch throw:KeyValueRest ->
        KeyValueRest
    end.

-doc #{ equiv => format(Format, []) }.
-spec format(format()) -> unicode:unicode_binary().
format(Format) ->
    format(Format, []).

-doc #{ equiv => format(Format, [], []) }.
-spec format(format(), Data :: [term()]) -> unicode:unicode_binary().
format(Format, Data) ->
    format(Format, Data, []).

-doc """
Returns a character list that represents `Data` formatted in accordance with
`Format`.

This function works just as `io_lib:bformat/2`, except that it also allows
atoms and tuples represeting virtual terminal sequences as part of the
`Format` string.

Example:

```erlang
1> io_ansi:format([blue, underline, "Hello world"]).
~"\e[34m\e[4mHello world\e(B\e[m"
```

For a detailed description of the available formatting options, see `io:fwrite/3`.
""".
-spec format(format(), Data :: [term()], options()) -> unicode:unicode_binary().
format(Format, Data, Options) ->
    format_internal(Format, Data, Options).

format_internal(Format, Data, Options) ->
    UseAnsi = case proplists:get_value(enabled, Options) of
                    undefined -> enabled();
                    Enabled -> Enabled
              end,
    %% Only to be used by fwrite
    FormatOnly = proplists:get_value(format_only, Options, false),
    AppendReset = [reset || proplists:get_value(reset, Options, true)],
    Mappings = get_mappings(),
    try lists:foldl(
          fun(Ansi, {Acc, Args}) when is_atom(Ansi) orelse is_tuple(Ansi),
                                      UseAnsi, FormatOnly ->
                  {[Ansi | Acc], Args};
             (Ansi, {Acc, Args}) when is_atom(Ansi), UseAnsi ->
                AnsiFun = lookup(Mappings, Ansi, 0),
                  {[AnsiFun() | Acc], Args};
             (Ansi, {Acc, Args}) when is_tuple(Ansi), UseAnsi ->
                [AnsiCode | AnsiArgs] = tuple_to_list(Ansi),
                AnsiFun = lookup(Mappings, AnsiCode, length(AnsiArgs)),
                  {[apply(AnsiFun, AnsiArgs) | Acc], Args};
             (Ansi, {Acc, Args}) when is_atom(Ansi); is_tuple(Ansi) ->
                  {Acc, Args};
             (Fmt, {Acc, Args}) ->
                  {Scanned, Rest} = io_lib_format:scan(Fmt, Args),
                  {[io_lib_format:build_bin(Scanned) | Acc], Rest}
          end, {[], Data}, group(lists:flatten([Format,AppendReset]))) of
        {Scanned, []} ->
            if FormatOnly ->
                    lists:flatten(lists:reverse(Scanned));
               not FormatOnly ->
                    unicode:characters_to_binary(lists:reverse(Scanned))
            end;
        _ ->
            erlang:error(badarg, [Format, Data, Options])
    catch E:R:ST ->
            erlang:raise(E,R,ST)
            %%            erlang:error(badarg, [Format, Data, Options])
    end.

-doc #{ equiv => fwrite(standard_io, Format, [], []) }.
-spec fwrite(Format :: format()) -> ok.
fwrite(Format) ->
    fwrite(standard_io, Format, [], []).

-doc #{ equiv => fwrite(standard_io, Format, Data, []) }.
-spec fwrite(Format :: format(), [term()]) -> ok.
fwrite(Format, Data) ->
    fwrite(standard_io, Format, Data, []).

-doc #{ equiv => fwrite(standard_io, Format, Data, Options) }.
-spec fwrite(Format :: format(), [term()], options()) -> ok.
fwrite(Format, Data, Options) ->
    fwrite(standard_io, Format, Data, Options).
-doc """
Writes the items in `Data` on the [`IoDevice`](`t:io:device/0`) in accordance with `Format`.

This function works just as `io:fwrite/2`, except that it also allows atoms and
tuples representing virtual terminal sequences (VTS) as part of the `Format` string.

Example:

```erlang
1> io_ansi:fwrite([blue, "%% Hello world\n"]).
%% Hello world
ok
```

The decision what each VTS should be converted to is done by the destination I/O
device. This means that if the I/O device is on a remote node, the terminfo
database loaded into that remote node will be used.

All VTSs are stripped if the target I/O device does not support handling VTSs,
either because it is not implemented by the device (for example if the device
is a `t:file:io_server/0`) or if the device does not support a certain VTS.
If you want to force usage of VTSs you can pass `{enabled,true}` and that will
use the local defintions to translate.
""".
-spec fwrite(IODevice :: io:device(), Format :: format(), [term()], options()) -> ok.
fwrite(Device, Format, Data, Options) ->
    Ref = make_ref(),
    try
        lists:foldl(
          fun F(Chars, ok) when is_binary(Chars) ->
                  io:request(Device, {put_chars, unicode, Chars});
              F(Ansi, ok) when is_atom(Ansi); is_tuple(Ansi) ->
                  case io:request(Device,{put_ansi, Options, Ansi}) of
                      {error, request} ->
                          %% The IO server did not support printing ansi.
                          case proplists:get_value(enabled, Options, Ref) of
                              true ->
                                  %% If ansi is forced by the ansi option,
                                  %% we format using the local
                                  %% ansi definition and send as characters
                                  F(format([Ansi], [], Options), ok);
                              Ref ->
                                  %% We drop the ansi codes
                                  ok
                          end;
                      Else -> Else
                  end;
              F(_Data, Error) ->
                  throw({Ref, Error})
          end, ok, format_internal(Format, Data, [{format_only, true} | Options]))
    catch {Ref, Error} ->
            erlang:error(Error, [Device, Format, Data, Options])
    end.

group(Fmt) ->
    group(Fmt, []).
group([Ansi | T], []) when is_atom(Ansi); is_tuple(Ansi) ->
    [Ansi | group(T, [])];
group([Ansi | T], Acc) when is_atom(Ansi); is_tuple(Ansi) ->
    [lists:reverse(Acc), Ansi | group(T, [])];
group([C | T], Acc) ->
    group(T, [C | Acc]);
group([], []) ->
    [];
group([], Acc) ->
    [lists:reverse(Acc)].

sgr(Args) -> ["\e[",lists:join($;,Args),"m"].

lookup(Key, Arity) ->
    lookup(get_mappings(), Key, Arity).
lookup(Mappings, Key, Arity) ->
    case maps:get(Arity, maps:get(Key, Mappings)) of
        #{ terminfo := TermInfoFun } -> TermInfoFun;
        #{ ansi := AnsiFun } -> AnsiFun
    end.

get_mappings() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            persistent_term:put(?MODULE, init_mappings()),
            get_mappings();
        Value -> Value
    end.

init_mappings() ->
    maps:map(
      fun
          Map(Key, Mapping) when not is_list(Mapping) ->
                    Map(Key, [Mapping]);
          Map(_Key, Mappings) when is_list(Mappings) ->
                    maps:from_list(
                      lists:map(
                        fun
                            Fun({undefined, AnsiFun}) when is_function(AnsiFun) ->
                                       {arity, Arity} = erlang:fun_info(AnsiFun, arity),
                                       {Arity, #{ ansi => AnsiFun }};
                            Fun({undefined, AnsiString}) when not is_function(AnsiString) ->
                                       Fun({undefined, fun() -> AnsiString end});
                            Fun({TermInfoCap, AnsiFun}) when is_function(AnsiFun) ->
                                       try prim_tty:tigetstr(TermInfoCap) of
                                           {ok, TermInfoStr} ->
                                               {arity, Arity} = erlang:fun_info(AnsiFun, arity),
                                               TermInfoFun =
                                                   case Arity of
                                                       0 -> fun() -> {ok, S} = prim_tty:tputs(TermInfoStr, []), S end;
                                                       1 -> fun(A) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A]), S end;
                                                       2 -> fun(A, B) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B]), S end;
                                                       3 -> fun(A, B, C) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B, C]), S end;
                                                       4 -> fun(A, B, C, D) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B, C, D]), S end;
                                                       5 -> fun(A, B, C, D, E) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B , C, D, E]), S end;
                                                       6 -> fun(A, B, C, D, E, F) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F]), S end;
                                                       7 -> fun(A, B, C, D, E, F, G) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F, G]), S end;
                                                       8 -> fun(A, B, C, D, E, F, G, H) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F, G, H]), S end;
                                                       9 -> fun(A, B, C, D, E, F, G, H, I) -> {ok, S} =prim_tty:tputs(TermInfoStr, [A, B , C, D, E, F, G, H, I]), S end
                                                   end,
                                               { Arity, #{ terminfo => TermInfoFun, ansi => AnsiFun }};
                                           false -> Fun({undefined, AnsiFun})
                                       catch error:badarg ->
                                               Fun({undefined, AnsiFun})
                                       end;
                            Fun({{TermInfoCap, Args}, AnsiString}) when not is_function(AnsiString) ->
                                       try prim_tty:tigetstr(TermInfoCap) of
                                           {ok, S} ->
                                               {ok, TermInfoStr} = prim_tty:tputs(S, Args),
                                               {0, #{ terminfo => fun() -> TermInfoStr end,
                                                      ansi => fun() -> AnsiString end } };
                                           false ->
                                               Fun({undefined, AnsiString})
                                       catch error:badarg ->
                                               Fun({undefined, AnsiString})
                                       end;
                            Fun({TermInfoCap, AnsiString})  when not is_function(AnsiString) ->
                                       Fun({{TermInfoCap, []}, AnsiString})
                               end, Mappings))
            end, default_mappings()).

get_vts_mappings() ->
    case persistent_term:get(io_ansi_vts_mappings, undefined) of
        undefined ->
            persistent_term:put(io_ansi_vts_mappings, init_vts_mappings()),
            get_vts_mappings();
        Value -> Value
    end.

init_vts_mappings() ->
    maps:map(fun Map(Key, Value) when not is_list(Value) ->
                     Map(Key, [Value]);
                 Map(_Key, Values) when is_list(Values) ->
                     lists:flatmap(fun F({_TermInfoCap, Fun}) when is_function(Fun) ->
                                           [];
                                       F({undefined, Ansi}) ->
                                           [unicode:characters_to_binary(Ansi)];
                                       F({{TermInfoCap, Args}, Ansi}) ->
                                           try prim_tty:tigetstr(TermInfoCap) of
                                               {ok, S} ->
                                                   {ok, TermInfoStr} = prim_tty:tputs(S, Args),
                                                   [unicode:characters_to_binary(TermInfoStr),
                                                    unicode:characters_to_binary(Ansi)];
                                               undefined ->
                                                   F({undefined, Ansi})
                                           catch error:badarg ->
                                                   F({undefined, Ansi})
                                           end;
                                       F({TermInfoCap, Ansi}) ->
                                           F({{TermInfoCap, []}, Ansi})
                                   end, Values)
             end, default_mappings()).

default_mappings() ->
    #{ cursor =>
           [{"cup", fun(Line, Column) -> ["\e[",Line+1,";",Column+1,"H"] end}],
       cursor_home => {"home", "\eH"},
       cursor_up =>
           [{"cuu1", "\e[A"},
            {"cuu", fun(Steps) -> ["\e[", Steps ,"A"] end}],

       cursor_down =>
           [{"cud1", "\n"},
            {"cud", fun(Steps) -> ["\e[", Steps ,"B"] end}],

       cursor_backward =>
           [{"cub1", "\b"},
            {"cub", fun(Steps) -> ["\e[", Steps ,"D"] end}],

       cursor_forward =>
           [{"cuf1", "\e[C"},
            {"cuf", fun(Steps) -> ["\e[", Steps ,"C"] end}],

       reverse_index => { "ri", "\eM" },
       cursor_save => { "sc", "\e7" },
       cursor_restore => { "rc", "\e8" },
       cursor_show => { "cvvis", "\e[?25h"},
       cursor_hide => { "civis", "\e[?25l"},

       cursor_next_line => { "nel", "\e[E" },
       cursor_previous_line => { undefined, "\e[F" },
       cursor_horizontal_absolute => { "hpa", fun(X) -> ["\e[", X, "G"] end},
       cursor_vertical_absolute => { "vpa", fun(Y) -> ["\e[", Y, "d"] end},
       cursor_horizontal_vertical => { undefined, fun(X, Y) -> ["\e[", X, ";", Y, "f"] end},

       alternate_screen => { "smcup", "\e[?1049h" },
       alternate_screen_off => { "rmcup", "\e[?1049l" },

       scroll_forward => [{{"indn", [1]}, "\eS"},
                          { "indn", fun(Steps) -> ["\e", Steps, "S"] end}],
       scroll_backward => [{{"rin", [1]}, "\eT"},
                           { "rin", fun(Steps) -> ["\e", Steps, "T"] end}],
       scroll_change_region => {"csr", fun(Line1, Line2) -> ["\e[",Line1,";",Line2,"r"] end},

       insert_character => { "ich", fun(Chars) -> ["\e[", Chars, "@"] end},
       delete_character => [{"dch1", "\e[P"},
                            { "dch", fun(Chars) -> ["\e[", Chars, "P"] end}],
       erase_character => { "ech", fun(Chars) -> ["\e[", Chars, "X"] end},
       insert_line => [{"il1", "\e[L"},
                       { "il", fun(Chars) -> ["\e[", Chars, "L"] end}],
       delete_line => [{"dl1", "\e[M"},
                       { "dl", fun(Chars) -> ["\e[", Chars, "M"] end}],

       erase_display => { "ed", "\e[J"},
       erase_line => {"el", "\e[K"},
       clear => { "clear", "\e[H\e[2J"},

       modify_color => { "initc", fun(Index, R, G, B) ->
                                          io_lib:format("\e]4;~.16b;rgb:~.16b/~.16b/~.16b\e\\",[Index, R, G, B])
                                  end},

       keypad_transmit_mode => { "smkx", "\e=" },
       keypad_transmit_mode_off => { "rmkx", "\e>" },

       kcursor_home => {"khome", "\eH"},
       kcursor_end => {"kend", "\eF"},
       kcursor_up => {"kcuu1", "\e[A"},
       kcursor_down => {"kcud1", "\e[B"},
       kcursor_backward => {"kcub1", "\e[D"},
       kcursor_forward => {"kcuf1", "\e[C"},

       cursor_report_position => { "u7", "\e[6n" },
       device_report_attributes => { undefined, "\e[0c"},

       tab_set => { "hts", "\eH" },
       tab => { "ht", "\e[I" },
       tab_backward => { "cbt", "\eI" },
       tab_clear => { undefined, "\e[0g" },
       tab_clear_all => { "tbc", "\e[3g" },

       reset => { "sgr0", sgr(["0"])},

       black => { {"setaf", [0]}, sgr(["30"]) },
       red => { {"setaf", [1]}, sgr(["31"]) },
       green => { {"setaf", [2]}, sgr(["32"]) },
       yellow => { {"setaf", [3]}, sgr(["33"]) },
       blue => { {"setaf", [4]}, sgr(["34"]) },
       magenta => { {"setaf", [5]}, sgr(["35"]) },
       cyan => { {"setaf", [6]}, sgr(["36"]) },
       white => { {"setaf", [7]}, sgr(["37"]) },
       default_color => { undefined, sgr(["39"]) },
       color => [{"setaf", fun(Index) -> sgr(["38","5",Index]) end},
                 {undefined, fun(R, G, B) -> sgr(["38","2",R,G,B]) end}],

       black_background => { {"setab", [0]}, sgr(["40"]) },
       red_background => { {"setab", [1]}, sgr(["41"]) },
       green_background => { {"setab", [2]}, sgr(["42"]) },
       yellow_background => { {"setab", [3]}, sgr(["43"]) },
       blue_background => { {"setab", [4]}, sgr(["44"]) },
       magenta_background => { {"setab", [5]}, sgr(["45"]) },
       cyan_background => { {"setab", [6]}, sgr(["46"]) },
       white_background => { {"setab", [7]}, sgr(["47"]) },
       default_background => { undefined, sgr(["49"]) },
       background => [{"setab", fun(Index) -> sgr(["48","5",Index]) end},
                      {undefined, fun(R, G, B) -> sgr(["48","2",R,G,B]) end}],

       light_black => { {"setaf", [8]}, sgr(["90"]) },
       light_red => { {"setaf", [9]}, sgr(["91"]) },
       light_green => { {"setaf", [10]}, sgr(["92"]) },
       light_yellow => { {"setaf", [11]}, sgr(["93"]) },
       light_blue => { {"setaf", [12]}, sgr(["94"]) },
       light_magenta => { {"setaf", [13]}, sgr(["95"]) },
       light_cyan => { {"setaf", [14]}, sgr(["96"]) },
       light_white => { {"setaf", [15]}, sgr(["97"]) },

       light_black_background => { {"setab", [8]}, sgr(["100"]) },
       light_red_background => { {"setab", [9]}, sgr(["101"]) },
       light_green_background => { {"setab", [10]}, sgr(["102"]) },
       light_yellow_background => { {"setab", [11]}, sgr(["103"]) },
       light_blue_background => { {"setab", [12]}, sgr(["104"]) },
       light_magenta_background => { {"setab", [13]}, sgr(["105"]) },
       light_cyan_background => { {"setab", [14]}, sgr(["106"]) },
       light_white_background => { {"setab", [15]}, sgr(["107"]) },

       bold => { "bold", "\e[1m" },
       bold_off => { undefined, "\e[22m" },
       underline => { "smul", "\e[4m" },
       underline_off => { "rmul", "\e[24m" },
       negative => { "smso", "\e[7m"},
       negative_off => { "rmso", "\e[27m"},

       alternate_character_set_mode => {"smacs", "\e(0" },
       alternate_character_set_mode_off => {"rmacs", "\e(B" },

       hyperlink =>
           [{undefined, hyperlink("", [])},
            {undefined, fun(Url) -> hyperlink(Url, []) end},
            {undefined, fun(Url, Params) -> hyperlink(Url, Params) end}]
     }.

hyperlink(URL, Params) ->
    StringParams = lists:join($:, [[K, "=", V] || {K, V} <- Params]),
    io_lib:format("\e]8;~s;~s\e\\",[URL, StringParams]).