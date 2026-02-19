%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(shell_docs_markdown_SUITE).

%% callbacks
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).

%% test format
-export([convert_erlang_html/1, convert_unknown_format/1]).

%% test non-existing moduledoc
-export([non_existing_moduledoc/1,hidden_moduledoc/1,existing_moduledoc/1]).

%% test non-existing docs
-export([non_existing_doc/1, hidden_doc/1, existing_doc/1]).

%% headings
-export([h1_test/1, h2_test/1, h3_test/1, h4_test/1, h5_test/1, h6_test/1,
         setext_h1/1, setext_h2/1, atx_h1_with_attrs_line/1, setext_h1_with_attrs_line/1]).

%% quotes
-export([single_line_quote_test/1, double_char_for_quote_test/1,
         ignore_three_spaces_before_quote/1, multiple_line_quote_test/1,
         paragraph_in_between_test/1, quote_with_anchor_test/1, quote_without_space/1,
         quote_with_attrs_line/1]).

%% paragraph
-export([paragraph_after_heading_test/1, quote_before_and_after_paragraph_test/1,
         paragraph_with_attrs_line/1]).

%% inline code
-export([single_line_code_test/1, multiple_line_code_test/1, paragraph_between_code_test/1,
         indented_code_with_attrs_line/1]).

%% fence code
-export([single_line_fence_code_test/1, multiple_line_fence_code_test/1,
         single_line_fence_code_no_language_test/1, single_line_fence_code_no_language_spaces_test/1,
         paragraph_between_fence_code_test/1, fence_code_ignores_link_format_test/1,
         fence_code_with_spaces/1, fence_code_with_tabs/1,
         fence_code_with_attrs_line/1]).

%% br
-export([start_with_br_test/1, multiple_br_followed_by_paragraph_test/1,
         multiple_lines_of_a_paragraph_test/1, ending_br_test/1]).

%% Comments
-export([begin_comment_test/1, after_paragraph_comment/1, forget_closing_comment/1 ]).

%% Format
-export([format_heading_test/1, format_paragraph_test/1, format_multiple_inline/1,
         format_multiple_inline_format_short/1, format_multiple_inline_format_long/1,
         format_multiple_inline_format_mixed/1, unmatched_format_simple/1,
         unmatched_format_with_inline/1, unmatched_complex_format_with_inline/1,
         format_inline_link_with_inline/1, complex_inline_format/1, skip_symbols_in_inline/1,
         format_header_identifier/1, italic_in_middle_word_test/1, italic_with_colons/1,
         markdown_attrs_line/1, markdown_attrs_ignored_in_inline_code/1,
         inline_italic_with_attrs_suffix/1, inline_code_with_attrs_suffix/1,
         inline_em_with_attrs_suffix/1, inline_link_with_attrs_suffix/1,
         inline_key_value_attrs_suffix/1, inline_all_attr_variants_suffix/1,
         block_key_value_attrs_line/1, block_all_attr_variants_line/1,
         commonmark_spec_json_compat/1,
         list_format_with_italics_in_sentence/1, list_format_with_bold_in_sentence/1,
         new_lines_test/1, format_separator_test/1, list_with_format/1, multi_word_format_test/1,
         multiline_link/1, multiline_link_not_allowed/1, inline_mfa_link/1,
         escaped_character/1, parens_with_italics/1]).

%% Bullet lists
-export([singleton_bullet_list/1, singleton_bullet_list_followed_new_paragraph/1, singleton_bullet_list_with_format/1,
         singleton_bullet_list_followed_inner_paragraph/1, singleton_bullet_list_followed_inner_paragraph2/1,
         singleton_bullet_list_followed_inner_paragraph3/1, multiline_bullet_indented_list/1, multiline_bullet_indented_list2/1,
         multiline_bullet_list/1, even_nested_bullet_list/1, odd_nested_bullet_list/1,
         complex_nested_bullet_list/1, complex_nested_bullet_list2/1, complex_nested_bullet_list3/1,
         bullet_list_mix_with_number_list/1, inline_code_list/1, bullet_list_with_anchor/1,
         bullet_list_with_attrs_line/1]).

%% Numbered lists
-export([singleton_numbered_list/1, singleton_numbered_list_followed_new_paragraph/1,
         singleton_numbered_list_with_format/1, singleton_numbered_list_followed_inner_paragraph/1,
         singleton_numbered_list_followed_inner_paragraph2/1, multiline_numbered_indented_list/1,
         multiline_numbered_indented_list2/1, multiline_numbered_list/1, even_nested_numbered_list/1,
         odd_nested_numbered_list/1, numbered_list_with_attrs_line/1]).

-export([table_with_rows/1, table_with_escaped_bars/1, fake_table_test/1,
         table_with_attrs_line/1]).

-define(ERLANG_HTML, ~"application/erlang+html").

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").


-define(EXPECTED_FUN(Expected), {{function, foo, 0}, [], [], Expected, #{}}).

all() ->
    [{group, different_format_generator},
     {group, module_generator},
     {group, doc_generator},
     {group, header_generator},
     {group, quote_generator},
     {group, paragraph_generator},
     {group, code_generator},
     {group, fence_code_generator},
     {group, br_generator},
     {group, comment_generator},
     {group, format_generator},
     {group, bullet_list_generator},
     {group, numbered_list_generator},
     {group, table_generator}
    ].

groups() ->
    [{different_format_generator, [sequence], different_format_conversion_tests()},
     {module_generator, [sequence], moduledoc_tests()},
     {doc_generator, [sequence], doc_tests()},
     {header_generator, [sequence], header_tests()},
     {quote_generator, [sequence], quote_tests()},
     {paragraph_generator, [sequence], paragraph_tests()},
     {code_generator, [sequence], code_tests()},
     {fence_code_generator, [sequence], fence_code_tests()},
     {br_generator, [sequence], br_tests()},
     {comment_generator, [sequence], comment_tests()},
     {format_generator, [sequence], format_tests()},
     {bullet_list_generator, [sequence], bullet_list_tests()},
     {numbered_list_generator, [sequence], numbered_list_tests()},
     {table_generator, [sequence], table_tests()}
    ].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

different_format_conversion_tests() ->
    [ convert_erlang_html,
      convert_unknown_format
    ].

moduledoc_tests() ->
    [ non_existing_moduledoc,
      hidden_moduledoc,
      existing_moduledoc
    ].

doc_tests() ->
    [ non_existing_doc,
      hidden_doc,
      existing_doc
    ].

header_tests() ->
    [ h1_test,
      h2_test,
      h3_test,
      h4_test,
      h5_test,
      h6_test,
      setext_h1,
      setext_h2,
      atx_h1_with_attrs_line,
      setext_h1_with_attrs_line
    ].

quote_tests() ->
    [ single_line_quote_test,
      double_char_for_quote_test,
      ignore_three_spaces_before_quote,
      multiple_line_quote_test,
      paragraph_in_between_test,
      quote_with_anchor_test,
      quote_without_space,
      quote_with_attrs_line
    ].

paragraph_tests() ->
    [ paragraph_after_heading_test,
      quote_before_and_after_paragraph_test,
      paragraph_with_attrs_line
    ].

code_tests() ->
    [ single_line_code_test,
      multiple_line_code_test,
      paragraph_between_code_test,
      indented_code_with_attrs_line
    ].

fence_code_tests() ->
  [single_line_fence_code_test,
   multiple_line_fence_code_test,
   single_line_fence_code_no_language_test,
   single_line_fence_code_no_language_spaces_test,
   paragraph_between_fence_code_test,
   fence_code_ignores_link_format_test,
   fence_code_with_spaces, fence_code_with_tabs,
   fence_code_with_attrs_line
  ].

br_tests() ->
    [ start_with_br_test,
      multiple_br_followed_by_paragraph_test,
      multiple_lines_of_a_paragraph_test,
      ending_br_test
    ].

comment_tests() ->
    [ begin_comment_test,
      after_paragraph_comment,
      forget_closing_comment
    ].

format_tests() ->
    [ format_heading_test,
      format_paragraph_test,
      format_multiple_inline,
      format_multiple_inline_format_long,
      format_multiple_inline_format_short,
      format_multiple_inline_format_mixed,
      unmatched_format_simple,
      unmatched_format_with_inline,
      unmatched_complex_format_with_inline,
      format_inline_link_with_inline,
      complex_inline_format,
      skip_symbols_in_inline,
      format_header_identifier,
      markdown_attrs_line,
      markdown_attrs_ignored_in_inline_code,
      inline_italic_with_attrs_suffix,
      inline_code_with_attrs_suffix,
      inline_em_with_attrs_suffix,
      inline_link_with_attrs_suffix,
      inline_key_value_attrs_suffix,
      inline_all_attr_variants_suffix,
      block_key_value_attrs_line,
      block_all_attr_variants_line,
      commonmark_spec_json_compat,
      italic_in_middle_word_test,
      italic_with_colons,
      list_format_with_italics_in_sentence,
      list_format_with_bold_in_sentence,
      new_lines_test,
      format_separator_test,
      list_with_format,
      multi_word_format_test,
      multiline_link,
      multiline_link_not_allowed,
      inline_mfa_link,
      escaped_character,
      parens_with_italics
    ].

bullet_list_tests() ->
    [ singleton_bullet_list,
      singleton_bullet_list_followed_new_paragraph,
      singleton_bullet_list_with_format,
      singleton_bullet_list_followed_inner_paragraph,
      singleton_bullet_list_followed_inner_paragraph2,
      singleton_bullet_list_followed_inner_paragraph3,
      multiline_bullet_indented_list,
      multiline_bullet_indented_list2,
      multiline_bullet_list,
      even_nested_bullet_list,
      odd_nested_bullet_list,
      complex_nested_bullet_list,
      complex_nested_bullet_list2,
      complex_nested_bullet_list3,
      bullet_list_mix_with_number_list,
      inline_code_list,
      bullet_list_with_anchor,
      bullet_list_with_attrs_line
    ].

numbered_list_tests() ->
    [ singleton_numbered_list,
      singleton_numbered_list_followed_new_paragraph,
      singleton_numbered_list_with_format,
      singleton_numbered_list_followed_inner_paragraph,
      singleton_numbered_list_followed_inner_paragraph2,
      multiline_numbered_indented_list,
      multiline_numbered_indented_list2,
      multiline_numbered_list,
      even_nested_numbered_list,
      odd_nested_numbered_list,
      numbered_list_with_attrs_line
    ].

table_tests() ->
  [ table_with_rows,
    table_with_escaped_bars,
    fake_table_test,
    table_with_attrs_line].

convert_erlang_html(_Conf) ->
    Doc = #{~"en" => [{p, [], [~"Test"]}]},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}}],

    Docs = create_eep48(erlang, ?ERLANG_HTML, none, #{}, Functions),
    ok = shell_docs:validate(Docs),
    ok.

convert_unknown_format(_Conf) ->
    Doc = #{~"en" => ~"<text>Here</text>"},
    Functions = create_fun(Doc),

    Docs = create_eep48(erlang, ~"xml", Doc, #{},Functions),
    ok = try
           shell_docs:validate(Docs)
         catch
           error:function_clause ->
             ok
         end,
    ok.

non_existing_moduledoc(_Conf) ->
    Docs = create_eep48(erlang, ~"application/erlang+html", none, #{}, []),
    _ = compile(Docs),
    ok.

hidden_moduledoc(_Conf) ->
    Docs = create_eep48(erlang, ~"application/erlang+html", hidden, #{}, []),
    _ = compile(Docs),
    ok.

existing_moduledoc(_Conf) ->
    Docs = create_eep48_doc(~"# Here"),
    HtmlDocs = compile(Docs),
    #{~"en" := HtmlModDoc} = extract_moduledoc(HtmlDocs),
    H1 = header(1, ~"Here"),
    [H1] = HtmlModDoc,
    ok.

non_existing_doc(_Conf) ->
    Docs = create_eep48(erlang, ~"application/erlang+html", none, #{}, create_fun(none)),
    ok = shell_docs:validate(Docs),
    ok.

hidden_doc(_Conf) ->
    Docs = create_eep48(erlang, ~"application/erlang+html", none, #{}, create_fun(hidden)),
    ok = shell_docs:validate(Docs),
    ok.

existing_doc(_Conf) ->
    Docs = create_eep48_doc(~"Test"),
    ok = shell_docs:validate(Docs),
    ok.

h1_test(_Conf) ->
    Input = ~"# Here",
    Result = [header(1, ~"Here")],
    compile_and_compare(Input, Result).

h2_test(_Conf) ->
    Input = ~"# Here\n## Header 2",
    Result = [header(1, ~"Here"), header(2,~"Header 2")],
    compile_and_compare(Input, Result).

h3_test(_Conf) ->
    Input = ~"# Here\n### Header 3",
    Result = [header(1, ~"Here"), header(3, ~"Header 3")],
    compile_and_compare(Input, Result).

h4_test(_Conf) ->
    Input = ~"### Here\n#### Header 4",
    Result = [header(3, ~"Here"), header(4, ~"Header 4")],
    compile_and_compare(Input, Result).

h5_test(_Conf) ->
    Convert = fun shell_docs_markdown:parse_md/1,
    Doc = #{~"en" => Convert(~"### Here\n#### Header 4\n##### H5")},
    DocH5 = #{~"en" => Convert(~"##### H5")},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}},
                 {{function, bar, 0}, [], [], DocH5, #{}}],
    Docs = create_eep48(erlang, ~"application/erlang+html", Doc, #{}, Functions),

    HtmlDocs = compile(Docs),
    ExpectedH3 = #{~"en" => [ header(3, ~"Here"),
                              header(4, ~"Header 4"),
                              header(5, ~"H5")]},
    ExpectedH5 = #{~"en" => [ header(5, ~"H5") ]},
    ExpectedH3 = extract_moduledoc(HtmlDocs),
    [ E1, E2 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], ExpectedH3, #{}} = E1,
    {{function, bar, 0}, [], [], ExpectedH5, #{}} = E2,
    ok.

h6_test(_Conf) ->
    Convert = fun shell_docs_markdown:parse_md/1,
    Doc = #{~"en" => Convert(~"### Here\n#### Header 4\n##### H5")},
    DocH6 = #{~"en" => Convert(~"###### H6\n## H2")},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}},
                 {{function, bar, 0}, [], [], DocH6, #{}}],
    Docs = create_eep48(erlang, ~"application/erlang+html", Doc, #{}, Functions),

    HtmlDocs = compile(Docs),
    ExpectedH3 = #{~"en" => [ header(3, ~"Here"),
                              header(4, ~"Header 4"),
                              header(5, ~"H5")]},
    ExpectedH6 = #{~"en" => [ header(6, ~"H6"),
                              header(2, ~"H2")]},
    ExpectedH3 = extract_moduledoc(HtmlDocs),
    [ E1, E2 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], ExpectedH3, #{}} = E1,
    {{function, bar, 0}, [], [], ExpectedH6, #{}} = E2,
    ok.

setext_h1(_Config) ->
    Input = ~"Here\n===\n\nNew text",
    Result = [ header(1, ~"Here"),
               p(~"New text")],
    compile_and_compare(Input, Result).

setext_h2(_Config) ->
    Input = ~"Here\n--\n\nNew text",
    Result = [ header(2, ~"Here"),
               p(~"New text")],
    compile_and_compare(Input, Result).

atx_h1_with_attrs_line(_Config) ->
    Input = ~"# Here\n{: #id .class }\n\nBody",
    Result = [{h1, [{class, ~"class"}, {id, ~"id"}], [~"Here"]},
              p(~"Body")],
    compile_and_compare(Input, Result).

setext_h1_with_attrs_line(_Config) ->
    Input = ~"Here\n===\n{: #id .class }\n\nBody",
    Result = [{h1, [{class, ~"class"}, {id, ~"id"}], [~"Here"]},
              p(~"Body")],
    compile_and_compare(Input, Result).

single_line_quote_test(_Conf) ->
    Input = ~"# Here\n> This is a quote",
    Result = [ header(1, ~"Here"),
               blockquote(p(~"This is a quote"))],
    compile_and_compare(Input, Result).

double_char_for_quote_test(_Conf) ->
    Input = ~"# Here\n>> This is a quote",
    Result = [ header(1, ~"Here"),
               blockquote(p(~"This is a quote"))],
    compile_and_compare(Input, Result).

ignore_three_spaces_before_quote(_Conf) ->
    Input = ~"   > # Here",
    Result = blockquote([header(1, ~"Here")]),
    compile_and_compare(Input, Result).

multiple_line_quote_test(_Conf) ->
    Input = ~"> # Here\n> This is a quote",
    Result = [ blockquote([header(1, ~"Here"),
                           p(~"This is a quote")])],
  compile_and_compare(Input, Result).

paragraph_in_between_test(_Conf) ->
    Input = ~"# Header 1\nThis is text\n> A quote\n> continues\n## Header 2\nBody content",
    Result = [ header(1, ~"Header 1"),
               p(~"This is text"),
               blockquote([p(~"A quote continues")]),
               header(2, ~"Header 2"),
               p(~"Body content")],
    compile_and_compare(Input, Result).

quote_with_anchor_test(_Config) ->
    Input =
~"> #### Note{: .info }
>
> The [User's Guide](index.html) has examples and a
> [Getting Started](using_ssh.md) section.",
    Result = [blockquote([header(4,~"Note"),
                                     p([~"The User's Guide has examples and a Getting Started section."])])],
    compile_and_compare(Input, Result).

quote_without_space(_Config) ->
    Input =
~"> #### Note{: .info }
>
>The [User's Guide](index.html) has examples and a
> [Getting Started](using_ssh.md) section.",
    Result = [blockquote([header(4,~"Note"),
                                     p([~"The User's Guide has examples and a Getting Started section."])])],
    compile_and_compare(Input, Result).

quote_with_attrs_line(_Config) ->
    Input = ~"> First line\n> {: .info }\n> Second line",
    Result = [blockquote([{p, [{class, ~"info"}], [~"First line"]},
                          p(~"Second line")])],
    compile_and_compare(Input, Result).

paragraph_after_heading_test(_Conf) ->
    Input = ~"# Header 1\nThis is text\n\nBody content",
    Result = [ header(1, ~"Header 1"),
               p(~"This is text"),
               p(~"Body content")],
    compile_and_compare(Input, Result).

paragraph_with_attrs_line(_Config) ->
    Input = ~"Paragraph text\n\n{: .lead }\n\nNext paragraph",
    Result = [{p, [{class, ~"lead"}], [~"Paragraph text"]},
              p(~"Next paragraph")],
    compile_and_compare(Input, Result).

quote_before_and_after_paragraph_test(_Conf) ->
    Input = ~"> Quote 1\nThis is text\n> Quote 2\nBody content",
    Result = [ blockquote(p(~"Quote 1")),
               p(~"This is text"),
               blockquote(p(~"Quote 2")),
               p(~"Body content")],
    compile_and_compare(Input, Result).

single_line_code_test(_Conf) ->
    Input = ~"# Here\n    This is code",
    Result = [ header(1, ~"Here"),
               code(~"This is code\n")],
    compile_and_compare(Input, Result).

multiple_line_code_test(_Conf) ->
    Input = ~"    # Here\n    This is code\n        Nested Line",
    Result = code(~"# Here\nThis is code\n    Nested Line\n"),
    compile_and_compare(Input, Result).

paragraph_between_code_test(_Conf) ->
    Input = <<"This is a paragraph\n",
              "\n",
              "    # Here\n",
              "    This is code\n",
              "        Nested Line\n",
              "Another paragraph">>,
    Result = [ p(~"This is a paragraph"),
               code(~"# Here\nThis is code\n    Nested Line\n"),
               p(~"Another paragraph")],
    compile_and_compare(Input, Result).

indented_code_with_attrs_line(_Config) ->
    Input = ~"    code()\n\n{: .snippet }\n\nAfter",
    Result = [{pre, [{class, ~"snippet"}], [{code, [], [~"code()\n"]}]},
              p(~"After")],
    compile_and_compare(Input, Result).

single_line_fence_code_test(_Conf) ->
    Input = ~"
```erlang
test() -> ok.
```",
    Result = [ code(~"test() -> ok.\n", [{class, ~"language-erlang"}])],
    compile_and_compare(Input, Result).

multiple_line_fence_code_test(_Conf) ->
    Input = ~"
```erlang
test() ->
  ok.
```",
    Result = [ code(~"test() ->\n  ok.\n", [{class, ~"language-erlang"}])],
    compile_and_compare(Input, Result).

single_line_fence_code_no_language_test(_Conf) ->
    Input = ~"
```
test() -> ok.
```",
    Result = [ code(~"test() -> ok.\n")],
    compile_and_compare(Input, Result).

single_line_fence_code_no_language_spaces_test(_Conf) ->
    Input = ~"
```\s\s
test() -> ok.
```",
    Result = [ code(~"test() -> ok.\n")],
    compile_and_compare(Input, Result).

paragraph_between_fence_code_test(_Conf) ->
    Input = ~"This is a test:
```erlang
test() ->
  ok.
```",
    Result = [p(~"This is a test:"),
                         code(~"test() ->\n  ok.\n", [{class, ~"language-erlang"}])],
    compile_and_compare(Input, Result).

fence_code_ignores_link_format_test(_Conf) ->
    Input = ~"This is a test:
```erlang
[foo](bar)
```",
    Result = [p(~"This is a test:"),
              code(~"[foo](bar)\n", [{class, ~"language-erlang"}])],
    compile_and_compare(Input, Result).

fence_code_with_spaces(_Config) ->
    Input =
~"  ```erlang\s\s
  [foo](bar)
```",
    Result = [code(~"  [foo](bar)\n", [{class, ~"language-erlang"}])],
    compile_and_compare(Input, Result).

fence_code_with_tabs(_Config) ->
    Input =
~"  ```erlang\ttrailing
  [foo](bar)
```",
    Result = [code(~"  [foo](bar)\n", [{class, ~"language-erlang"}])],
    compile_and_compare(Input, Result).

fence_code_with_attrs_line(_Config) ->
    Input = ~"
```erlang
test() -> ok.
```
{: .erl-code }
Next paragraph",
    Result = [code(~"test() -> ok.\n", [{class, ~"language-erlang erl-code"}]),
              p(~"Next paragraph")],
    compile_and_compare(Input, Result).

start_with_br_test(_Conf) ->
    Input = ~"\n\nAnother paragraph",
    Result = [ p(~"Another paragraph")],
    compile_and_compare(Input, Result).

multiple_br_followed_by_paragraph_test(_Conf) ->
    Input = ~"\nAnother paragraph\n\nAnother paragraph",
    Result = [ p(~"Another paragraph"),
               p(~"Another paragraph")],
    compile_and_compare(Input, Result).

multiple_lines_of_a_paragraph_test(_Conf) ->
  Input = ~"""
Returns a new list `List3`, which is made from the elements of `List1` followed
by the elements of `List2`.
""",
    Result = [p([~"Returns a new list ",
                 inline_code(~"List3"),
                 ~", which is made from the elements of ",
                 inline_code(~"List1"), ~" followed by the elements of ",
                 inline_code(~"List2"), ~"."])],
    compile_and_compare(Input, Result).

ending_br_test(_Conf) ->
    Input = ~"Test\n",
    Result = [ p(~"Test")],
    compile_and_compare(Input, Result).

begin_comment_test(_Conf) ->
    Input = ~"<!-- Ignore -->Test",
    Result = [ p(~"Test")],
    compile_and_compare(Input, Result).

after_paragraph_comment(_Conf) ->
    Input = ~"Test\n<!-- Ignore -->Test",
    Result = [ p(~"Test"), p(~"Test")],
    compile_and_compare(Input, Result).

forget_closing_comment(_Conf) ->
    ok = try
           create_eep48_doc(~"Test\n<!-- Ignore Test")
         catch
           error:missing_close_comment ->
             ok
         end,
    ok.

format_heading_test(_Conf) ->
    Input = ~"# **H1**\n## _H2_\n### `H3`",
    Result = [ header(1, em(~"H1")),
               header(2, it(~"H2")),
               header(3, inline_code(~"H3"))
             ],
    compile_and_compare(Input, Result).

format_paragraph_test(_Conf) ->
    Input = ~"**H1** *HH* _H2_ __H3__ `code`",
    Result = [ p([ em(~"H1"),
                   ~" ",
                   it(~"HH"),
                   ~" ",
                   it(~"H2"),
                   ~" ",
                   em(~"H3"),
                   ~" ",
                   inline_code(~"code")
                 ])
             ],
    compile_and_compare(Input, Result).

format_multiple_inline(_Conf) ->
    Input = ~"**H1 *HH***",
    Result = [ p([ em([~"H1 ", it(~"HH")])]) ],
    compile_and_compare(Input, Result).

format_multiple_inline_format_long(_Conf) ->
    Input = ~"**H1 __HH__**",
    Result = [ p([ em([~"H1 ", em(~"HH")])]) ],
    compile_and_compare(Input, Result).

format_multiple_inline_format_short(_Conf) ->
    Input = ~"_H1 *HH*_",
    Result = [ p([ it([~"H1 ", it(~"HH")])])],
    compile_and_compare(Input, Result).

format_multiple_inline_format_mixed(_Conf) ->
    Input = ~"__H1 *HH* _test_ **test2**__ _there_",
    Result = [ p([ em([~"H1 ", it(~"HH"), ~" ", it(~"test"), ~" ", em(~"test2")]),
                   ~" ",
                   it(~"there")])
             ],
    compile_and_compare(Input, Result).

unmatched_format_simple(_Conf) ->
    Input = ~"**Bold*",
    Result = [ p([~"**Bold*" ])],
    compile_and_compare(Input, Result).

unmatched_format_with_inline(_Conf) ->
    Input = ~"**Bold *Italics*",
    Result = p([~"**Bold ", it(~"Italics") ]),
    compile_and_compare(Input, Result).

unmatched_complex_format_with_inline(_Conf) ->
    Input = ~"__H1 *HH* _test_ **test2** _there_ ",
    Result = [ p([ ~"__H1 ", it(~"HH"), ~" ", it(~"test"), ~" ",
                   em(~"test2"),
                   ~" ",
                   it(~"there"), ~" "])
             ],
    compile_and_compare(Input, Result).

format_inline_link_with_inline(_Config) ->
    Input = ~"[`splitwith/2`](`splitwith/2`) behaves as if",
    Result = [ p([inline_code(~"splitwith/2"),~" behaves as if"])],
    compile_and_compare(Input, Result).

complex_inline_format(_Config) ->
  %% The complexity here comes from the _, where the alg needs to backtrack
  %% add the _ to an existing buffer, and continue where it left off using  %% any previous opening format characters, in this case **.
  Input = ~"**`{set_alarm, {AlarmId, AlarmDescr}}`**",
  Result = p(em(inline_code(~"{set_alarm, {AlarmId, AlarmDescr}}"))),
  compile_and_compare(Input, Result).

italic_in_middle_word_test(_Config) ->
  Input = ~"this ssh_daemon_channel is not in italics",
  Result = p(~"this ssh_daemon_channel is not in italics"),
  compile_and_compare(Input, Result).

italic_with_colons(_Config) ->
  Input = ~"_This is ok:_ **test:**",
  Result = p([it(~"This is ok:"), ~" ", em(~"test:")]),
  compile_and_compare(Input, Result).

list_format_with_italics_in_sentence(_Config) ->
  Input = ~"- Mandatory: one or more _Host key(s)_. Default is `/etc/ssh`",
  Result = ul([li(p([~"Mandatory: one or more ",
                     it(~"Host key(s)"),
                     ~". Default is ",
                     inline_code(~"/etc/ssh")]))]),
  compile_and_compare(Input, Result).

list_format_with_bold_in_sentence(_Config) ->
  Input = ~"- Mandatory: one or more __Host key(s)__. Default is `/etc/ssh`",
  Result = ul([li(p([~"Mandatory: one or more ",
                     em(~"Host key(s)"),
                     ~". Default is ",
                     inline_code(~"/etc/ssh")]))]),
  compile_and_compare(Input, Result).

format_separator_test(_Config) ->
  Input = ~"**This is a Test,**, including _a parens)_ and **This is a Test**, end",
  Result = p([em(~"This is a Test,"),
              ~", including ",
              it(~"a parens)"),
              ~" and ",
              em(~"This is a Test"),
              ~", end"
             ]),
  compile_and_compare(Input, Result).

multi_word_format_test(_Config) ->
  Input = ~"**`--help` (or `-h`)** - Print this message and exit.",
  Result = p([em([inline_code(~"--help"),
                  ~" (or ",
                  inline_code(~"-h"),
                  ~")"]),
              ~" - Print this message and exit."]),
  compile_and_compare(Input, Result).


multiline_link(_Config) ->
  Input = ~"this is a [link with\nnew line](erlang.com)",
  Result = p([~"this is a link with new line"]),
  compile_and_compare(Input, Result).

%% As per commonmark.js-0.28.1, Github Flavoured Markdown 0.23.10
multiline_link_not_allowed(_Config) ->
  Input = ~"this is a [link with new line]\n(erlang.com)",
  Result = p([~"this is a [link with new line] (erlang.com)"]),
  compile_and_compare(Input, Result).

inline_mfa_link(_Config) ->
  Input = ~"See `t:count()`, and `c:arith:plus/1`, `e:math:sum/2`, and `m:extra:here/1`",
  Result = p([~"See ", inline_code(~"count()"), ~", and ",
              inline_code(~"arith:plus/1"), ~", ",
              inline_code(~"math:sum/2"), ~", and ",
              inline_code(~"extra:here/1")]),
  compile_and_compare(Input, Result).

escaped_character(_Config) ->
  Input = ~B"Here \*not bold\* and \`not code\` and \_means_nothing_ `\`test\``",
  Result = p([~"Here *not bold* and `not code` and _means_nothing_ ",
              inline_code(~"`test`")]),
  compile_and_compare(Input, Result).

parens_with_italics(_Config) ->
  Input = ~"Test ( _Optional_). ( _Optional_ .) ( _Optional_ ). (_Optional_). (_Optional._)",
  Result = p([~"Test ( ", it(~"Optional"), ~"). ( "
                        , it(~"Optional"), ~" .) ( "
                        , it(~"Optional"), ~" ). ("
                        , it(~"Optional"), ~"). ("
                        , it(~"Optional."), ~")"]),
  compile_and_compare(Input, Result),

  Input1 = ~"Test ( __Optional__). ( __Optional__ .) ",
  Input2 = ~"( __Optional__ ). (__Optional__). (__Optional.__)",
  InputEm = <<Input1/binary, Input2/binary>>,
  ResultEm = p([~"Test ( ", em(~"Optional"), ~"). ( "
                          , em(~"Optional"), ~" .) ( "
                          , em(~"Optional"), ~" ). ("
                          , em(~"Optional"), ~"). ("
                          , em(~"Optional."), ~")"]),
  compile_and_compare(InputEm, ResultEm).

list_with_format(_Config) ->
  Input = ~"- **`--help` (or `-h`)** - Print this message and exit.",
  Result = ul([li(p([em([inline_code(~"--help"),
                         ~" (or ",
                         inline_code(~"-h"),
                         ~")"]),
                     ~" - Print this message and exit."]))]),
  compile_and_compare(Input, Result).

new_lines_test(_Config) ->
  Input = ~"Render in
the same line",
  Result = p([~"Render in the same line"]),
  compile_and_compare(Input, Result).

skip_symbols_in_inline(_Config) ->
  Input = ~"**`{the_beginning, the_end}`**",
  Result = p(em(inline_code(~"{the_beginning, the_end}"))),
  compile_and_compare(Input, Result).

format_header_identifier(_Config) ->
  Input = ~"## Test {: #id .class} there",
  Result = header(2, ~"Test  there"),
  compile_and_compare(Input, Result).

markdown_attrs_line(_Config) ->
  Input = ~"Text before\n{: #id .class }\nText after",
  Result = p(~"Text before  Text after"),
  compile_and_compare(Input, Result).

block_key_value_attrs_line(_Config) ->
  Input = ~"Text before\n\n{: title=Guide }\n\nText after",
  Result = [{p, [{title, ~"Guide"}], [~"Text before"]},
            p(~"Text after")],
  compile_and_compare(Input, Result).

block_all_attr_variants_line(_Config) ->
  Input = ~"Text before\n\n{: .cls #ident title=\"Guide\" }\n\nText after",
  Result = [{p, [{class, ~"cls"}, {id, ~"ident"}, {title, ~"Guide"}], [~"Text before"]},
            p(~"Text after")],
  compile_and_compare(Input, Result).

commonmark_spec_json_compat(Config) ->
  DataDir = proplists:get_value(data_dir, Config),
  SpecPath = filename:join(DataDir, ~"spec.json"),
  SpecBin = element(2, file:read_file(SpecPath)),
  SpecExamples = json:decode(SpecBin),

  {Pass, Fail, FailingExamples, FailingSections} =
      lists:foldl(
        fun(Example, {P, F, Fails, SecFails}) ->
            Markdown = maps:get(~"markdown", Example),
            ExpectedHtml = maps:get(~"html", Example),
            ExampleId = maps:get(~"example", Example),
            Section = maps:get(~"section", Example),
            Parsed = shell_docs_markdown:parse_md(Markdown),
            ActualHtml = markdown_ast_to_html(Parsed),
            case ActualHtml =:= ExpectedHtml of
                true ->
                    {P + 1, F, Fails, SecFails};
                false ->
                    Count = maps:get(Section, SecFails, 0),
                    {P, F + 1, [ExampleId | Fails], SecFails#{Section => Count + 1}}
            end
        end, {0, 0, [], #{}}, SpecExamples),

  ct:log("commonmark_spec_json_compat: pass=~p fail=~p total=~p",
         [Pass, Fail, length(SpecExamples)]),
  ct:log("commonmark_spec_json_compat: failing sections=~p",
         [lists:sort(maps:to_list(FailingSections))]),
  case Fail of
      0 ->
          ok;
      _ ->
          First = lists:sublist(lists:reverse(FailingExamples), 20),
          ct:fail({commonmark_examples_failed, pass, Pass, fail, Fail,
                   total, length(SpecExamples), first_failing_examples, First})
  end.

markdown_ast_to_html(Elems) when is_list(Elems) ->
    iolist_to_binary([chunk_to_html(E) || E <- Elems]).

chunk_to_html({pre, Attrs, [{code, CodeAttrs, Chunks}]}) ->
    [~"<pre", attrs_to_html(Attrs), ~"><code", attrs_to_html(CodeAttrs), ~">",
     inline_to_html(Chunks), ~"</code></pre>\n"];
chunk_to_html({blockquote, Attrs, Chunks}) ->
    [~"<blockquote", attrs_to_html(Attrs), ~">\n", markdown_ast_to_html(Chunks), ~"</blockquote>\n"];
chunk_to_html({ul, Attrs, Chunks}) ->
    [~"<ul", attrs_to_html(Attrs), ~">\n", markdown_ast_to_html(Chunks), ~"</ul>\n"];
chunk_to_html({ol, Attrs, Chunks}) ->
    [~"<ol", attrs_to_html(Attrs), ~">\n", markdown_ast_to_html(Chunks), ~"</ol>\n"];
chunk_to_html({li, Attrs, Chunks}) ->
    case has_block_children(Chunks) of
        true ->
            [~"<li", attrs_to_html(Attrs), ~">\n", markdown_ast_to_html(Chunks), ~"</li>\n"];
        false ->
            [~"<li", attrs_to_html(Attrs), ~">", inline_to_html(Chunks), ~"</li>\n"]
    end;
chunk_to_html({Tag, Attrs, Chunks}) when Tag =:= p; Tag =:= h1; Tag =:= h2; Tag =:= h3;
                                         Tag =:= h4; Tag =:= h5; Tag =:= h6 ->
    TagBin = atom_to_binary(Tag),
    [~"<", TagBin, attrs_to_html(Attrs), ~">", inline_to_html(Chunks), ~"</", TagBin, ~">\n"].

inline_to_html(Chunks) when is_list(Chunks) ->
    [inline_chunk_to_html(C) || C <- Chunks].

inline_chunk_to_html({i, Attrs, Chunks}) ->
    [~"<em", attrs_to_html(Attrs), ~">", inline_to_html(Chunks), ~"</em>"];
inline_chunk_to_html({em, Attrs, Chunks}) ->
    [~"<em", attrs_to_html(Attrs), ~">", inline_to_html(Chunks), ~"</em>"];
inline_chunk_to_html({code, Attrs, Chunks}) ->
    [~"<code", attrs_to_html(Attrs), ~">", inline_to_html(Chunks), ~"</code>"];
inline_chunk_to_html(Bin) when is_binary(Bin) ->
    escape_html(Bin).

attrs_to_html([]) ->
    [];
attrs_to_html(Attrs) ->
    [~" ", lists:join(~" ", [attr_to_html(A) || A <- Attrs])].

attr_to_html({Key, Value}) ->
    [atom_to_binary(Key), ~"=\"", escape_attr(Value), ~"\""].

escape_html(Bin) when is_binary(Bin) ->
    R1 = binary:replace(Bin, ~"&", ~"&amp;", [global]),
    R2 = binary:replace(R1, ~"<", ~"&lt;", [global]),
    binary:replace(R2, ~">", ~"&gt;", [global]).

escape_attr(Bin) when is_binary(Bin) ->
    binary:replace(escape_html(Bin), ~"\"", ~"&quot;", [global]).

has_block_children(Chunks) ->
    lists:any(fun is_block_chunk/1, Chunks).

is_block_chunk({Tag, _, _}) when Tag =:= p; Tag =:= blockquote; Tag =:= pre; Tag =:= ul; Tag =:= ol;
                                 Tag =:= li; Tag =:= h1; Tag =:= h2; Tag =:= h3; Tag =:= h4; Tag =:= h5;
                                 Tag =:= h6 ->
    true;
is_block_chunk(_) ->
    false.

markdown_attrs_ignored_in_inline_code(_Config) ->
  Input = ~"`{: #id .class }`",
  Result = p(inline_code(~"{: #id .class }")),
  compile_and_compare(Input, Result).

inline_italic_with_attrs_suffix(_Config) ->
  Input = ~"_equal_{: #equal }",
  Result = p({i, [{id, ~"equal"}], [~"equal"]}),
  compile_and_compare(Input, Result).

inline_code_with_attrs_suffix(_Config) ->
  Input = ~"`term`{: .inline-code }",
  Result = p({code, [{class, ~"inline-code"}], [~"term"]}),
  compile_and_compare(Input, Result).

inline_em_with_attrs_suffix(_Config) ->
  Input = ~"**name**{: .n }",
  Result = p({em, [{class, ~"n"}], [~"name"]}),
  compile_and_compare(Input, Result).

inline_link_with_attrs_suffix(_Config) ->
  Input = ~"[label](https://example.org){: #ref }",
  Result = p([~"label"]),
  compile_and_compare(Input, Result).

inline_key_value_attrs_suffix(_Config) ->
  Input = ~"_label_{: title=\"Guide\" }",
  Result = p({i, [{title, ~"Guide"}], [~"label"]}),
  compile_and_compare(Input, Result).

inline_all_attr_variants_suffix(_Config) ->
  Input = ~"_label_{: .cls #ident title=Guide }",
  Result = p({i, [{class, ~"cls"}, {id, ~"ident"}, {title, ~"Guide"}], [~"label"]}),
  compile_and_compare(Input, Result).

singleton_bullet_list(_Config) ->
    Input = ~"* One liner",
    Result = [ul([li(p(~"One liner"))])],
    compile_and_compare(Input, Result).

singleton_bullet_list_followed_new_paragraph(_Config) ->
    Input = ~"* One liner\n\nThis is a new paragraph",
    Result = [ul([li(p(~"One liner"))]), p(~"This is a new paragraph")],
    compile_and_compare(Input, Result).

singleton_bullet_list_followed_inner_paragraph(_Config) ->
    Input = ~"* One liner\n  This is a new paragraph",
    Result = [ul([li([p([~"One liner This is a new paragraph"])])])],
    compile_and_compare(Input, Result).

singleton_bullet_list_followed_inner_paragraph2(_Config) ->
    Input = ~"* One liner.\nThis is a new paragraph",
    Result = [ul([li([p([~"One liner. This is a new paragraph"])])])],
    compile_and_compare(Input, Result).

singleton_bullet_list_followed_inner_paragraph3(_Config) ->
    Input = ~"* One liner\n\n  This is a new paragraph\n  continue here\n\n  and this one follows\n\nNew text",
    Result = [ul([li([p(~"One liner"),
                      p([~"This is a new paragraph continue here"]),
                      p(~"and this one follows")])]),
              p(~"New text")],
    compile_and_compare(Input, Result).

singleton_bullet_list_with_format(_Config) ->
    Input = ~"* *One* __liner__",
    Result = [ul([li(p([it(~"One"), ~" ",  em(~"liner")]))])],
    compile_and_compare(Input, Result).

multiline_bullet_list(_Config) ->
    Input = ~"* One liner\n* Second line",
    Result = [ul([li(p(~"One liner")), li(p(~"Second line"))])],
    compile_and_compare(Input, Result).

multiline_bullet_indented_list(_Config) ->
    Input = ~"  * One liner\n  * Second line",
    Result = [ul([li(p(~"One liner")), li(p(~"Second line"))])],
    compile_and_compare(Input, Result).

multiline_bullet_indented_list2(_Config) ->
    Input = ~"  * _One liner_\n  * _Second_ `line`",
    Result = [ul([li(p(it(~"One liner"))),
                  li(p([it(~"Second"), ~" ",  inline_code(~"line")]))])],
    compile_and_compare(Input, Result).

even_nested_bullet_list(_Config) ->
    Input = ~"* One liner\n  * First nested line\n  * Second nested line",
    Result = [ul([
                  li([ p(~"One liner"),
                       ul([ li(p(~"First nested line")),
                            li(p(~"Second nested line"))
                          ])
                     ])
                 ])],
    compile_and_compare(Input, Result).

odd_nested_bullet_list(_Config) ->
    Input = ~"* One liner\n  * First nested line\n  * Second nested line\n  * Third nested line",
    Result = [ul([
                  li([ p(~"One liner"),
                       ul([ li(p(~"First nested line")),
                            li(p(~"Second nested line")),
                            li(p(~"Third nested line"))
                          ])
                     ])
                 ])],
    compile_and_compare(Input, Result).

complex_nested_bullet_list(_Config) ->
    Input = ~"* One liner\n  * First nested line\n* Second line",
    Result = [ul([
                  li([ p(~"One liner"),
                       ul([ li(p(~"First nested line")) ])
                     ]),
                  li([p(~"Second line")])
                 ])],
    compile_and_compare(Input, Result).

complex_nested_bullet_list2(_Config) ->
    Input = ~"
* One liner
  * First nested line
    * Second level nested line
  * Second nested line
    * Another nested line
* Second one liner",
    Result = [ul([
                  li([ p(~"One liner"),
                       ul([ li([
                                p(~"First nested line"),
                                ul([ li([p(~"Second level nested line")])])
                               ]),
                            li([
                                p(~"Second nested line"),
                                ul([ li([p(~"Another nested line")])])
                               ])
                          ])
                     ]),
                  li([ p(~"Second one liner")])
                 ])],
    compile_and_compare(Input, Result).

complex_nested_bullet_list3(_Config) ->
    Input = ~"
- Optional: one or more _User's private key(s)_ in case of `publickey`
  authorization. The default files are
  - `id_dsa` and `id_dsa.pub`
  - `id_rsa` and `id_rsa.pub`
  - `id_ecdsa` and `id_ecdsa.pub`
",
    Result = [ul([ li([p([~"Optional: one or more ",
                          it(~"User's private key(s)"),
                          ~" in case of ",
                          inline_code(~"publickey"),
                           ~" authorization. The default files are"]),
                      ul([ li( [p([inline_code(~"id_dsa"),
                                ~" and ",
                                inline_code(~"id_dsa.pub")])]),
                           li( [p([inline_code(~"id_rsa"),
                                  ~" and ",
                                  inline_code(~"id_rsa.pub")])]),
                           li( [p([inline_code(~"id_ecdsa"),
                                ~" and ",
                               inline_code(~"id_ecdsa.pub")])])])])])],
    compile_and_compare(Input, Result).

bullet_list_mix_with_number_list(_Config) ->
    Input = ~"* Bullet list\n1. Numbered list",
    Result = [ul([li([ p(~"Bullet list")])]),
              ol([li([ p(~"Numbered list")])])],
    compile_and_compare(Input, Result).

inline_code_list(_Config) ->
    Input = ~"""
* ```
  Code block
    More code
  ```
  {: .class }
""",
    Result = [ul([li([code(~"Code block\n  More code\n", [{class, ~"class"}])])])],
    compile_and_compare(Input, Result).

%% this example could render the last line within the inner ul.
%% the end result looks exactly the same, and the fix is non-trivial.
bullet_list_with_anchor(_Config) ->
    Input = ~"""
- **`+c true | false`**{: #+c } - Enables or disables
  [time correction](time_correction.md#time-correction):

  - **`true`** - Enables time correction.
  - another example

  - **`false`** - Disables time correction.
""",
    Result = [ul([li([ p([{em, [{id, ~"+c"}], [inline_code(~"+c true | false")]},
                          ~" - Enables or disables time correction:"]),
                       ul([li([ p([ em(inline_code(~"true")), ~" - Enables time correction."])]),
                           li([ p(~"another example")])]),
                       ul([li(p([ em(inline_code(~"false")), ~" - Disables time correction."]))])])])],
    compile_and_compare(Input, Result).

bullet_list_with_attrs_line(_Config) ->
    Input = ~"* Bullet list\n\n{: .list-note }\n\nAfter",
    Result = [{ul, [{class, ~"list-note"}], [{li, [], [{p, [], [~"Bullet list"]}]}]},
              p(~"After")],
    compile_and_compare(Input, Result).

singleton_numbered_list(_Config) ->
    Input = ~"1. One liner",
    Result = [ol([li(p(~"One liner"))])],
    compile_and_compare(Input, Result).

singleton_numbered_list_followed_new_paragraph(_Config) ->
    Input = ~"1. One liner\n\nThis is a new paragraph",
    Result = [ol([li(p(~"One liner"))]), p(~"This is a new paragraph")],
    compile_and_compare(Input, Result).

singleton_numbered_list_followed_inner_paragraph(_Config) ->
    Input = ~"1. One liner\n  This is a new paragraph",
    Result = [ol([li([p([~"One liner This is a new paragraph"])])])],
    compile_and_compare(Input, Result).

singleton_numbered_list_followed_inner_paragraph2(_Config) ->
    Input = ~"1. One liner.\nThis is a new paragraph",
    Result = [ol([li([p([~"One liner. This is a new paragraph"])])])],
    compile_and_compare(Input, Result).

singleton_numbered_list_with_format(_Config) ->
    Input = ~"1. *One* __liner__",
    Result = [ol([li(p([it(~"One"), ~" ",  em(~"liner")]))])],
    compile_and_compare(Input, Result).

multiline_numbered_indented_list(_Config) ->
    Input = ~"  1. One liner\n  2. Second line",
    Result = [ol([li(p(~"One liner")), li(p(~"Second line"))])],
    compile_and_compare(Input, Result).

multiline_numbered_indented_list2(_Config) ->
    Input = ~"  1. _One liner_\n  2. _Second_ `line`",
    Result = [ol([li(p(it(~"One liner"))),
                  li(p([it(~"Second"), ~" ",  inline_code(~"line")]))])],
    compile_and_compare(Input, Result).

multiline_numbered_list(_Config) ->
    Input = ~"1. One liner\n2. Second line",
    Result = [ol([li(p(~"One liner")), li(p(~"Second line"))])],
    compile_and_compare(Input, Result).

even_nested_numbered_list(_Config) ->
    Input = ~"1. One liner\n  1. First nested line\n  2. Second nested line",
    Result = [ol([
                  li([ p(~"One liner"),
                       ol([ li(p(~"First nested line")),
                            li(p(~"Second nested line"))
                          ])
                     ])
                 ])],
    compile_and_compare(Input, Result).

odd_nested_numbered_list(_Config) ->
    Input = ~"1. One liner\n  1. First nested line\n  2. Second nested line\n  3. Third nested line",
    Result = [ol([
                  li([ p(~"One liner"),
                       ol([ li(p(~"First nested line")),
                            li(p(~"Second nested line")),
                            li(p(~"Third nested line"))
                          ])
                     ])
                 ])],
    compile_and_compare(Input, Result).

numbered_list_with_attrs_line(_Config) ->
    Input = ~"1. One liner\n\n{: .steps }\n\nAfter",
    Result = [{ol, [{class, ~"steps"}], [{li, [], [{p, [], [~"One liner"]}]}]},
              p(~"After")],
    compile_and_compare(Input, Result).


table_with_rows(_Config) ->
    Input = ~"""
| H1 | H2 | H3 |
|:-----|:--:|----|
|D1   | D2 | D3 |
""",
    Result = table([~"| H1 | H2 | H3 |\n",
                                ~"|:-----|:--:|----|\n",
                                ~"|D1   | D2 | D3 |\n"]),
    compile_and_compare(Input, Result).

table_with_escaped_bars(_Config) ->
    Input = ~"""
| **JSON** | **Erlang**             |
|----------|------------------------|
| Number   | `integer() \| float()` |
""",
    Result = table([~"| **JSON** | **Erlang**             |\n",
                    ~"|----------|------------------------|\n",
                    ~"| Number   | `integer() \\| float()` |\n"]),
    compile_and_compare(Input, Result).

fake_table_test(_Config) ->
    NotTable = ~"""
| **JSON** | **Erlang**
""",
    Result = [p([~"| ", em(~"JSON"), ~" | ", em(~"Erlang")])],
    compile_and_compare(NotTable, Result).

table_with_attrs_line(_Config) ->
    Input = ~"""
| H1 | H2 |
|----|----|
| A  | B  |

{: .data-table }

After
""",
    Result = [{pre, [{class, ~"data-table"}], [{code, [{class, ~"table"}], [~"| H1 | H2 |\n",
                                                                              ~"|----|----|\n",
                                                                              ~"| A  | B  |\n"]}]},
              p(~"After")],
    compile_and_compare(Input, Result).

header(Level, Text) when is_integer(Level) ->
    HeadingLevel = integer_to_list(Level),
    HeadingLevelAtom = list_to_existing_atom("h" ++ HeadingLevel),
    {HeadingLevelAtom, [], [Text]}.

code(X) ->
    code(X, []).
code(X, Attrs) when is_list(X) ->
    {pre,[],[{code,Attrs,X}]};
code(X, Attrs) ->
    {pre,[],[{code,Attrs,[X]}]}.

table(Table) when is_list(Table) ->
    {pre,[], [{code, [{class, ~"table"}], Table}]}.

inline_code(X) when is_list(X) ->
    {code,[],X};
inline_code(X) ->
    {code,[],[X]}.

p(X) when is_list(X) ->
    {p, [], X};
p(X) when is_tuple(X); is_binary(X) ->
    p([X]).

blockquote(X) when is_list(X) ->
  {blockquote, [], X};
blockquote(Tuple) when is_tuple(Tuple) ->
    {blockquote, [], [Tuple]}.

em(X) when is_list(X) ->
    {em, [], X};
em(X) when is_binary(X); is_tuple(X) ->
    em([X]).

it(X) when is_list(X) ->
    {i, [], X};
it(X) when is_binary(X); is_tuple(X) ->
    it([X]).

ul(Items) when is_list(Items) ->
    {ul, [], Items}.

ol(Items) when is_list(Items) ->
    {ol, [], Items}.

li(Item) when is_tuple(Item); is_binary(Item) ->
    li([Item]);
li(Items) when is_list(Items) ->
    {li, [], Items}.

-spec create_eep48(Language, Mime, ModuleDoc, Metadata, Docs) -> #docs_v1{} when
      Language  :: atom(),
      Mime      :: binary(),
      ModuleDoc :: #{DocLanguage := DocValue} | none | hidden,
      Metadata  :: map(),
      Docs      :: [{{Kind, Name, Arity},
                     Anno :: erl_anno:anno(),
                     Signature :: [binary()],
                     Doc :: #{DocLanguage := DocValue} | none | hidden,
                     Metadata :: map()
                    }],
      Kind      :: function | type | callback,
      Name      :: atom(),
      Arity     :: non_neg_integer(),
      DocLanguage :: binary(),
      DocValue :: binary() | term().
create_eep48(Language, Mime, ModuleDoc, Metadata, Docs) ->
    #docs_v1{anno = [],
             beam_language = Language,
             format = Mime,
             module_doc = ModuleDoc,
             metadata = maps:merge(#{ otp_doc_vsn => ?CURR_DOC_VERSION }, Metadata),
             docs = Docs}.

extract_moduledoc(Docs) ->
    Docs#docs_v1.module_doc.

extract_doc(Docs) ->
    Docs#docs_v1.docs.

extract_format(Docs) ->
    Docs#docs_v1.format.

create_eep48_doc(Doc) when is_binary(Doc) ->
    create_eep48_doc(Doc, ~"application/erlang+html").

create_eep48_doc(Doc, Format) when is_binary(Doc) ->
    Docs = #{~"en" => shell_docs_markdown:parse_md(Doc)},
    create_eep48(erlang, Format, Docs, #{}, create_fun(Docs)).

create_fun(Docs) ->
    [{{function, foo, 0}, [], [], Docs, #{}}].

expected(X) when is_list(X)->
    #{~"en" => X};
expected(X) when is_tuple(X) ->
    expected([X]).

compile(Docs) ->
    ok = shell_docs:validate(Docs),
    ?NATIVE_FORMAT = extract_format(Docs),
    Docs.

compile_and_compare(Input, Result) ->
    Docs = create_eep48_doc(Input),
    HtmlDocs = compile(Docs),

    Expected = expected(Result),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.
