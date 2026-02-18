# Markdown Parser CommonMark Gap Plan

The current `shell_docs_markdown` parser intentionally supports a subset of Markdown.
The following items are missing or only partially implemented compared to CommonMark.

## Missing or Partial Features

1. Preserve and render links as link elements
- Current behavior strips URL targets and keeps only link text.
- Implement proper link node parsing for inline links and references.

2. Image syntax support
- Add parsing for `![alt](url "title")` and reference-style images.

3. Raw HTML support
- Implement CommonMark HTML block and inline HTML handling rules.
- Keep existing comment handling behavior where appropriate.

4. Fenced code block parity
- Add support for tilde fences (`~~~`) in addition to backticks.
- Verify language info string handling stays compatible.

5. Ordered list marker coverage
- Expand list marker parsing beyond single-digit forms.
- Follow CommonMark list interruption and continuation rules more closely.

6. Thematic breaks
- Add support for horizontal rules (`---`, `***`, `___`) as block separators.

7. Reference-style link resolution
- Parse and resolve link reference definitions instead of only stripping them.

8. Autolinks and URI/email angle-bracket forms
- Add support for `<https://...>` and `<user@example.org>` autolinks.

9. Backslash escapes and punctuation parity
- Audit and align escape behavior with CommonMark expectations.

10. Table behavior is extension-specific and limited
- Current pipe-table support is custom and constrained.
- Decide whether to keep current behavior or align with GFM table semantics.

11. Attribute syntax is a custom extension
- `{: ... }` is supported for attrs but is non-CommonMark.
- Document extension boundaries and precedence with standard inline/block parsing.

## Validation Plan

1. Add parser tests for each missing feature before implementation.
2. Implement features incrementally behind small parser changes.
3. Run:
- `shell_docs_markdown_SUITE`
- `shell_docs_SUITE -case render`
- full `shell_docs_SUITE`
4. Compare renderer output snapshots to avoid unintentional regressions.
