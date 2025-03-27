#!/usr/bin/env escript
%% -*- erlang -*-

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

-include_lib("kernel/include/file.hrl").

main(Args) ->
    argparse:run(Args, cli(), #{ progname => 'license-header' }).

cli() ->
    #{ commands =>
           #{ "scan" =>
                  #{ help => "Scan files in folder for correct license headers.",
                     arguments => [verbose_option(),
                                   missing_option(),
                                   template_option(),
                                   spdx_option(),
                                   path_option()],
                     handler => fun scan/1},
              "update" =>
                  #{ help => "Update copyright on files in folder for correct license headers.",
                     arguments => [verbose_option(),
                                   missing_option(),
                                   template_option(),
                                   spdx_option(),
                                   path_option()],
                     handler => fun update/1}
            },
       help => "Help text"
     }.

template_option() ->
    #{ name => template,
       long => "-template",
       short => $t,
       default => filename:join([os:getenv("ERL_TOP"),"scripts","license-header-templates"]),
       help => "" }.

verbose_option() ->
    #{ name => verbose,
       long => "-verbose",
       short => $v,
       default => false,
       type => boolean,
       help => "" }.

spdx_option() ->
    #{ name => spdx,
       long => "-spdx",
       default => false,
       type => boolean,
       help => "" }.

missing_option() ->
    #{ name => missing,
       long => "-missing",
       default => false,
       type => boolean,
       help => "" }.

path_option() ->
    #{ name => path,
       required => true,
       nargs => list,
       help => "" }.

scan(Opts) ->
    FilesToScan = get_files_to_scan(Opts),
    [RootDir] = get_rootdir(Opts),
    LicenseTemplates = get_license_templates(maps:get(template, Opts)),
    VendorPaths = get_vendor_paths(RootDir),
    LargestLicense = lists:max([byte_size(L) || _ := L <- LicenseTemplates]),
    N = [check_file(File, LargestLicense * 2, LicenseTemplates, VendorPaths, Opts) || File <- FilesToScan],
    io:format("Checked ~p files\n", [length(N)]),
    ok.

check_file(File, LargestLicense, Templates, VendorPaths, Opts) ->
    try
        Data = read(File, LargestLicense),
        case re:run(Data, "(.*) %CopyrightBegin%(?:\r\n|\n)",[]) of
            {match, [{Start, StartEnd}, PrefixPos]} ->
                Prefix = binary:part(Data, PrefixPos),
                case re:run(Data, ["\\Q", Prefix, "\\E %CopyrightEnd%(\r\n|\n)"],[]) of
                    {match, [{End, EndPos},{_,NlSize}]} ->
                        DataAfterHeader = binary:part(Data, End+EndPos, byte_size(Data) - (End+EndPos)),
                        PrefixSpdxCopyrightAndLicense = binary:part(Data, Start+StartEnd, End - (Start+StartEnd) - NlSize),
                        SpdxCopyrightAndLicense = check_prefix(Prefix, PrefixSpdxCopyrightAndLicense),
                        {Spdx, CopyrightAndLicense} = check_spdx(SpdxCopyrightAndLicense, Opts),
                        License = check_copyright(CopyrightAndLicense),
                        check_license(License, Spdx, Templates, length(string:split(DataAfterHeader,"\n",all)), File, Opts);
                    nomatch when map_get(verbose, Opts) ->
                        throw({warn, "Could not find '~ts %CopyrightEnd%'.\n~ts", [Prefix, Data]});
                    nomatch ->
                        throw({warn, "Could not find '~ts %CopyrightEnd%'", [Prefix]})
                end;
            nomatch ->
                ReportMissing = not is_ignored(File) andalso
                    not is_vendored(File, VendorPaths)
                    andalso maps:get(missing, Opts),
                ReportMissing andalso throw({warn, "license header not found", []})
        end
    catch
        {fail, Fmt, Args} ->
            fail("~ts: " ++ Fmt, [File] ++ Args);
        {warn, Fmt, Args} ->
            warn("~ts: " ++ Fmt, [File] ++ Args);
        E:R:ST ->
            warn("~ts: crash", [File]),
            erlang:raise(E,R,ST)
    end.

check_prefix(Prefix, Bin) when is_binary(Bin) ->
    case string:split(Bin, "\r\n") of
        [Bin] ->
            Lines = string:split(Bin, "\n", all);
        [Line, Rest] ->
            Lines = [Line | string:split(Rest, "\r\n", all)]
    end,
    check_prefix(Prefix, Lines);
check_prefix(Prefix, [Line | Rest]) ->
    TrimmedPrefix = string:trim(Prefix, trailing),
    case Line of
        <<Prefix:(byte_size(Prefix))/binary, " ", Content/binary>> ->
            [Content | check_prefix(Prefix, Rest)];
        TrimmedPrefix ->
            [<<>> | check_prefix(Prefix, Rest)];
        Line ->
            throw({warn, "Incorrect prefix (~ts) on this line: '~p'", [Prefix, Line]})
    end;
check_prefix(_Prefix, []) ->
    [].

check_spdx([<<>>, <<"SPDX-License-Identifier: ", Spdx/binary>>, <<>> | Rest], _Opts) ->
    {Spdx, Rest};
check_spdx([<<>> | Rest], #{ spdx := false }) ->
    {~"Apache-2.0", Rest};
check_spdx(_, _) ->
    throw({warn, "Could not find 'SPDX-License-Identifier:'", []}).

check_copyright(Lines) ->
    check_copyright(Lines, []).
check_copyright([<<"SPDX-FileCopyrightText: Copyright ", Copyright/binary>> | Rest], Copyrights) ->
    check_copyright(Rest, [Copyright | Copyrights]);
check_copyright([<<"Copyright ", Copyright/binary>> | Rest], Copyrights) ->
    check_copyright(Rest, [Copyright | Copyrights]);
check_copyright([<<>> | _], []) ->
    throw({warn, "Could not find any 'Copyright ' statement", []});
check_copyright([<<>> | Rest], Copyrights) ->
    [case string:find(Copyright, "Ericsson AB") of
        nomatch -> ok;
        _ ->
            YearMatch = "(19|20)[0-9]{2}",
            case re:run(Copyright, ["^Ericsson AB (",YearMatch,"-)?(",YearMatch,")\\. "
                                   "All Rights Reserved\\.$"]) of
                nomatch -> throw({warn, "Invalid Ericsson Copyright: '~ts'", [Copyright]});
                _ -> ok
            end
     end || Copyright <- Copyrights],
    Rest;
check_copyright([Line | _], _Copyrights) ->
    throw({warn, "Copyright statements must start with 'Copyright ': ~ts", [Line]}).

check_license(License, Spdx, Templates, LinesAfterLicense, Filename, Opts) ->
    FlatSPDX = unicode:characters_to_binary(string:replace(Spdx, " ", "-", all)),
    check_license(lists:join($\n, License),
                  string:trim(maps:get(FlatSPDX, Templates)),
                  Opts#{ lines_after_license => LinesAfterLicense,
                         filename => Filename }).
check_license([], _, #{ lines_after_license := LinesAfterLicense, filename := Filename }) ->
    %% Regexps to run on filename that may have a short license
    ShortLicense = ["lib/[^/]+/test/[^/]+_SUITE_data/",
                    "\\.app(up)?\\.src$",
                    "vendor\\.info"],

    %% Regexps to run on filename that may have a short license
    %% if the file also is short
    ShortIfShort = [".*\\.mk$",
            "\\.spec$", "\\.cover$", "\\.exs$"],

    NeedsNoLicense = any_match(Filename, ShortLicense),
    
    IsShort = LinesAfterLicense =< 10,

    NeedsNoLicenseIfShort = any_match(Filename, ShortIfShort),

    if
        NeedsNoLicense -> ok;
        IsShort, NeedsNoLicenseIfShort -> ok;
        NeedsNoLicenseIfShort ->
            throw({warn, "is longer than 10 lines, needs license in header.", []});
        true ->
            throw({warn, "needs license in header.", []})
    end;
check_license(License, Template, Opts) ->
    TemplateWithNewLine = [Template,$\n],
    case string:equal(License, TemplateWithNewLine) of
        true -> ok;
        false when map_get(verbose, Opts) ->
            Tmp1 = cmd("mktemp"),
            Tmp2 = cmd("mktemp"),
            ok = file:write_file(Tmp1, License),
            ok = file:write_file(Tmp2, Template),
            Diff = cmd("diff " ++ Tmp1 ++ " " ++ Tmp2),
            ok = file:delete(Tmp1),
            ok = file:delete(Tmp2),
            throw({warn, "license header did not match template\n~ts", [Diff]});
        false ->
            throw({warn, "license header did not match template", []})
    end.

read(Filename, Bytes) ->
    {ok, D} = file:open(Filename, [read, raw, binary]),
    case file:read(D, Bytes) of
        eof -> {ok, Data} = file:read(D, 0);
        {ok, Data} -> Data
    end,
    file:close(D),
    Data.

update(Opts) ->
    io:format("~p~n",[Opts]),
    ok.

get_license_templates(Path) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            maps:from_list(
              lists:map(fun(FN) ->
                                {ok, Template} = file:read_file(filename:join(Path,FN)),
                                {unicode:characters_to_binary(string:replace(filename:rootname(FN)," ","-")), Template}
                        end, Files));
        _Err ->
            fail("Could not list ~ts~n",[Path])
    end.

get_vendor_paths(RootPath) ->
    lists:flatmap(fun get_vendor_path/1, filelib:wildcard(filename:join(RootPath, "**/vendor.info"))).
get_vendor_path(File) ->
    {ok, B} = file:read_file(File),
    case json:decode(re:replace(B, "^%.*", "", [multiline, global, {return, binary}])) of
        #{ ~"path" := Path } -> [filename:absname(Path)];
        Vendors ->
            lists:flatmap(
              fun(V) ->
                      case maps:get(~"path", V) of
                          Path when is_binary(Path) ->
                              [filename:absname(Path)];
                          Paths ->
                              [filename:absname(P) || P <- Paths]
                      end
              end, Vendors)
    end.

is_vendored(Filename, VendorPaths) ->
    lists:any(fun(Path) ->
         string:prefix(filename:absname(Filename), Path) =/= nomatch
    end, VendorPaths).

is_ignored(Filename) ->
    any_match(Filename,
              ["\\.beam$",
               "\\.gitignore$",
               "\\.gitattributes$",
               "\\.(ico|bmp|png|jpg)$",
               "prebuild\\.(skip|keep)$"]).

any_match(Filename, REs) ->
    lists:any(fun(RE) ->
                      case re:run(Filename, RE, [unicode]) of
                          {match, _} -> true;
                          _ -> false
                      end
              end, REs).

get_files_to_scan(#{ path := Paths }) ->
    lists:usort(
      lists:flatmap(
        fun(Path) ->
                case filelib:is_dir(Path) of
                    true -> get_files_from_dir(Path);
                    false ->
                        case filelib:is_regular(Path) of
                            true ->
                                [Path];
                            false ->
                                fail("~ts does not exist.", [Path])
                        end
                end
        end, Paths)).

get_rootdir(#{ path := Paths }) ->
    lists:usort(
      lists:map(
        fun(Path) ->
                DirPath =
                    case filelib:is_dir(Path) of
                        true -> Path;
                        false -> filename:dirname(Path)
                    end,
                string:trim(cmd("cd " ++ DirPath ++ " && git rev-parse --show-toplevel"))
        end, Paths)).

get_files_from_dir(Dir) ->
    Filenames = cmd("git ls-tree -z -r --name-only HEAD " ++ Dir),
    [Name || Name <- string:split(string:trim(Filenames, both, "\0"),"\0",all),
        not is_link(Name)].

is_link(Name) ->
    {ok, #file_info{ type = Type }} = file:read_link_info(Name),
    Type =/= regular.

                                                % prefix(Str, Prefix) ->
                                                %     case string:prefix(Str, Prefix) of
                                                %         nomatch -> unicode:characters_to_binary(Str);
                                                %         NoPrefix -> unicode:characters_to_binary(NoPrefix)
                                                %     end.

                                                % decode(Filename) ->
                                                %     {ok, Bin} = file:read_file(Filename),
                                                %     json:decode(Bin).

                                                % have_tools([H|T]) ->
                                                %     case os:find_executable(H) of
                                                %         false -> fail("Could not find '~ts' in PATH", [H]);
                                                %         _ -> have_tools(T)
                                                %     end;
                                                % have_tools([]) -> ok.

warn(Fmt, Args) ->
    io:format(standard_error, Fmt++"\n", Args).

fail(Fmt, Args) ->
    io:format(standard_error, Fmt++"\n", Args),
    do_cleanup(),
    erlang:halt(1).

                                                % cleanup(Fun) ->
                                                %     [put(cleanup, []) || get(cleanup) =:= undefined],
                                                %     put(cleanup, [Fun | get(cleanup)]).

do_cleanup() ->
    [put(cleanup, []) || get(cleanup) =:= undefined],
    [Fun() || Fun <- get(cleanup)],
    erase(cleanup).

cmd(Cmd) ->
    string:trim(os:cmd(unicode:characters_to_list(Cmd),
                       #{ exception_on_failure => true })).
