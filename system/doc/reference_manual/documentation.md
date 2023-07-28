# Documentation

> #### NOTE {: info}
>
> This document is not ready and contains a some implementation details about
> ex_doc which should not be part of the final product. We want to stay as
> format agnostic as we can here and then explain our markdown "extensions"
> separately.

Documentation in Erlang is done through the `-moduledoc` and `-doc` attributes. For example

```
-module(math).
-moduledoc "
A module for basic arithmetic.
".

-export([add/2]).

-doc "Adds two numbers together."
add(One, Two) -> One + Two.
```

The `-doc` attribute always precedes the function/type/callback it documents.
The `-moduledoc` attribute has to be located before any other `-doc` attribute.

By default the format used for documentation attributes is [Markdown][wikipedia]
but that can be changed by setting [module documentation metadata](#moduledoc-metadata).

A good starting point to writing Markdown is [Basic writing and formatting syntax][github].

If you do use markdown, then it is recommended to use the markdown flavor supported by
[Earmark][Earmark] as that makes it possible for ex_doc to generate html and epub documentation.

You do not have to have the documentation inline, if you want to you can also place
it in a file somewhere else. If you do so, then you can use `-doc {file, "path/to/doc.md"}`
to point to the documentation. The path used is relative to the file being documented. For example:

```markdown
%% doc/add.md
Adds two numbers together
```

```
%% src/math.erl
-doc {file,"../doc/add.md"}.
add(One, Two) -> One + Two.
```

[Earmark]: https://github.com/robertdober/earmark_parser
[wikipedia]: https://en.wikipedia.org/wiki/Markdown
[github]: https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax

## Documentation metadata

It is possible to add metadata to the documentation entry. You do this by adding
a `-moduledoc` or `-doc` attribute with a map as argument. For example:

```
-module(math).
-moduledoc "
A module for basic arithmetic.
".
-moduledoc #{ since => "1.0" }.

-export([add/2]).

-doc "Adds two number together."
-doc #{ since => "1.0" }.
add(One, Two) -> One + Two.
```

The metadata is used by documentation tools to provide extra information to
the user. You can have multiple metadata documentation entries, if you do
then the maps will be merged with the latest taking precedence if there are
duplicate keys. Example:

```
-doc "Adds two number together."
-doc #{ since => "1.0", author => "Joe" }.
-doc #{ since => "2.0" }.
add(One, Two) -> One + Two.
```

This will result in a metadata entry of `#{ since => "2.0", author => "Joe" }`.

## Documenting a module

The module description should include details on how to use the API and examples
of the different functions working together. Here is a good place to use images
and other diagrams to better show the usage of the module.

The moduledoc should start with a short paragraph describing the module and then
go into greater details.

### Moduledoc metadata

There are three reserved metadata keys for `-moduledoc`:

- `since` - Shows which version of the application the module was added.
- `deprecated` - Shows a text in the documentation explaining that it is deprecated and what to use instead.
- `format` - The format to use for all documentation in this module. The default is `text/markdown`.
  It should be written using the [mime type][] of the format.

Example:

```
-moduledoc {file, "../doc/math.asciidoc"}.
-moduledoc #{ since => "0.1", format => "text/asciidoc" }.
-moduledoc #{ deprecated => "Use the stdlib math module instead." }.
```

[mime type]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types

## Documenting a function / type / opauqe / callback

You can document functions, type and callbacks using the `-doc` attribute.
Each entry should start with a short paragraph describing the function/type/opaque/callback
and then go into greater detail in needed.

It is not a good idea to include images or diagrams in this documentation as
it is used by IDEs and `c:h/1` to show the documentation to the user.

For example:

````
-doc "
A number that can be used by the math module.

We use a special number here so that we know
that this number comes from this module.
".
-type number() :: {math, erlang:number()}.

-doc "
Adds two number together.

### Example:

```
1> math:add(math:number(1), math:number(2)).
{number, 3}
```
"
-spec add(number(), number()) -> number().
add({number, One}, {number, Two}) -> {number, One + Two}.
````

### Doc metadata

There are three reserved metadata keys for `-moduledoc`:

- `since` - Shows which version of the application the module was added.
- `deprecated` - Shows a text in the documentation explaining that it is deprecated and what to use instead.
- `equiv` - Notes that this function is equivalent to another function in this module. You can use either
  `Func/Arity` or `Func(Args)` to describe the equivalence. For example:
  
  ```
  -doc #{ equiv => add/3 }.
  add(One, Two) -> add(One, Two, []).
  add(One, Two, Options) -> ...
  ```
  
  or
  
  ```
  -doc #{ equiv => add(One, Two, []) }.
  -spec add(One :: number(), Two :: number()) -> number().
  add(One, Two) -> add(One, Two, []).
  add(One, Two, Options) -> ...
  ```
  
  If no documentation exists, documentation will be generated with a text pointing
  the user to `add/3`. In the second example there will be an addition check done
  by the compiler that `One` and `Two` are variable names in the `-spec` of the
  function.

### Doc slogans

The doc slogan is a short text shown to describe the function when hovering.
By default it is taken from the source code by looking at the names of the
arguments:

```
add(One, Two) -> One + Two.
```

will have a slogan of `add(One, Two)`. For types/opaques/callbacks,
the implementation will look at the type/opaque/callback
specification for what to use as slogan. For example:

```
-type number(Value) :: {number, Value}.
%% slogan will be `number(Value)`

-opaque number() :: {number, number()}.
%% slogan will be `number()`

-callback increment(In :: number()) -> Out.
%% slogan will be `increment(In)`

-callback increment(In) -> Out when
   In :: number().
%% slogan will be `increment(In)`
```

If it cannot "easily" figure out a nice slogan from the code, it will use the
MFA syntax instead, i.e. `add/2`, `number/1`, `increment/1` etc.

It is possible to supply your own slogan by placing it as the first line of
the `-doc` attribute. The provided slogan must be in the form of a function
declaration up until the `->`. For example:

```
-doc "
add(One, Two)

Adds two numbers.
"
add(A, B) -> A + B.
```

Will create the slogan `add(One, Two)`. This works for functions, types, opaques
and callbacks.

## Links

When writing documentation in markdown links are automatically found in any
inline code segment that looks like an MFA. For example:

```
-doc "See `sub/2` for more details"
```

will create a link to the `sub/2` function in the current module if it exists.
You can also use `` `sub/2` `` as the link target. For example:

```
-doc "See [subtract](`sub/2`) for more details"
-doc "See [`sub/2`] for more details"
-doc "See [subtract] for more details

[substract]: `sub/2`
"
-doc "See [subtract][1] for more details

[1]: `sub/2`
"
```

The above will all result in the same link being created.

The link can also be used to point to a bunch of other things:

- `remote functions` - Use `module:function/arity` syntax.
  
  Example:
  
  ```
  -doc "See `math:sub/2` for more details".
  ```
- `modules` - Write the module with a `m` prefix. You can also use anchors to
  jump to a specific place in the module.
  
  Example:
  
  ```
  -doc "See `m:math` for more details".
  -doc "See `m:math#anchor` for more details".
  ```
- `types` - Use same syntax as for local/remote function only add a `t` prefix.
  
  Example:
  
  ```
  -doc "See `t:number/0` for more details".
  -doc "See `t:math:number/0` for more details".
  ```
- `callbacks` - Use same syntax as for local/remote function only add a `c` prefix.
  
  Example:
  
  ```
  -doc "See `c:increment/0` for more details".
  -doc "See `c:math:increment/0` for more details".
  ```
- `pages` - For pages in the current application just use a normal link, i.e. "`[release notes](notes.md)`".
  For pages in another application you need to use the `p` prefix and state which
  application the page belongs to. You can also use anchors to jump to a specific
  place in the page.
  
  Example:
  
  ```
  -doc "See `p:stdlib:unicode_usage` for more details".
  -doc "See `p:stdlib:unicode_usage#notes-about-raw-filenames` for more details".
  ```

## Anchors

In `-moduledoc` and pages it can be useful to create anchors that can be linked
to. All headers have an id of their lowercase value with any special characters
replaced by `-`. For example "## Details about subtractions" becomes
"#details-about-subtractions".

Since we Earmark you can also use IAL to add custom anchors. For example:

```markdown
- `tag`{: #tag-anchor } - This is a taglist
```

Creates a tag list with an anchor called `tag-anchor`.

## Spellcheck and linting

> #### TODO {: info}
>
> I would like there to be someway to tell erlc to spellcheck/lint the
> docstrings. We want to be able to run both [markdown lint], [prettier] and [ispell]
> on the text. It would be easy to do if we use `-doc {file,...}` for all docs,
> but I'm not sure we want that....

[markdown lint]: https://github.com/markdownlint/markdownlint
[prettier]: https://prettier.io/
[ispell]: https://en.wikipedia.org/wiki/Ispell

## Things erl\_docgen/xml could do but ex\_doc/markdown cannot

There are a couple of things that were possible to do in erl_docgen but are
not possible with `-doc` attributes. Most of these are because markdown
does not support the feature and the HTML parser of [Earmark] does not work
very well at all.

There are also a lot of things that ex_doc/markdown can do that erl\_docgen/xml
cannot, but I will not list those.

### Definition lists

We have about 1000 `<taglist>` in our documentation, and GFM markdown does have
this syntax. Kramdown (another markdown flavor) does have syntax for it and
it seems like it is the most adopted one, but [Earmark] does not support it.

We could have used the raw html:

```markdown
<dl>
<dt>

Title

</dt>
<dd>

Text with [link].

</dd>
</dl>
```

but again [Earmark] does not handle this at all (although GFM does). I also
don't think using html is any good....

So instead we use the list syntax with the "Title" as a bold tag first in the list.
This is similar to how the Elixir team does things in their docs.

```markdown
- **Title** - Text with [link].
- `atom` - Text with [another link].
```

This works ok for most tag lists.

### pre sections

In erl\_docgen you can have links and emphasis in `<pre>` sections. For example
like is done in [logger:update\_formatter\_config/2]. This is not possible at all
in Markdown and would require raw HTML.

When translating I've just stripped any tags in such sections and changed it to
a code segment.

[logger:update\_formatter\_config/2]: https://www.erlang.org/doc/man/logger.html#update_formatter_config-2

### Annotations

It is not possible to do `<anno>` like things yet. But IMO ex_doc could/should be
extended for that purpose. Maybe `a:system_logger` or something like that.

### Inline types

All inline types (such as [erlang:system\_info(allocator)] have to become top level types.
This is less of a problem now as ex\_doc has hover for types so that you can preview types
in the browser. However for IDEs it is still impractical.

### man pages and pdf

ex_doc cannot generate docs for man pages and pdf's. Instead it generated for epub
and html only.

### Variable callback function names

For example `c:ct_suite:Testcase/0` and `c:ct_suite:Testcase/1`. We have no way to express
these in the -callback type language, so there is no way to express them in ex_doc either...

[erlang:system\_info(allocator)]: https://www.erlang.org/doc/man/erlang.html#system_info_allocator

### Document multi clause functions

If using `-doc` attributes we cannot document function clauses. Instead we need to rewrite the docs
for all function like I've done for `erlang:system_info/1` ([erlang_system_info.md]).

[erlang_system_info.md]: https://github.com/garazdawi/otp/blob/lukas/erl_docgen/eep48_to_markdown/erts/doc/src/erlang_system_info.md
