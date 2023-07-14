{global,_} = Code.eval_file Path.join(System.get_env("ERL_TOP"),"ex_doc.exs");
deps = Path.wildcard("lib/*/ebin") |> Enum.map(
  fn path ->
    app = Path.split(path) |> Enum.at(1);
    {String.to_atom(app), Path.join(["..",app])}
  end);
Keyword.merge(global,
  [
    extras: Path.wildcard("system/doc/*.md") ++
    Path.wildcard("system/doc/installation_guide/*.md") ++
    Path.wildcard("system/doc/system_principles/*.md") ++
    Path.wildcard("system/doc/embedded/*.md") ++
    Path.wildcard("system/doc/getting_started/*.md") ++
    Path.wildcard("system/doc/reference_manual/*.md") ++
    Path.wildcard("system/doc/programming_examples/*.md") ++
    Path.wildcard("system/doc/efficiency_guide/*.md") ++
    Path.wildcard("system/doc/tutorial/*.md") ++
    Path.wildcard("system/doc/design_principles/*.md") ++
    Path.wildcard("system/doc/oam/*.md"),
    main: "readme",
    api_reference: false,
    groups_for_extras: [
      "Installation Guide": ~r/installation_guide/,
      "Getting Started With Erlang": ~r/getting_started/,
      "System Principles": ~r/system_principles/,
      "OTP Design Principles": ~r/design_principles/,
      "Programming Examples": ~r/programming_examples/,
      "Erlang Reference Manual": ~r/reference_manual/,
      "Efficiency Guide": ~r/efficiency_guide/,
      "Interoperability Tutorial": ~r/tutorial/,
      "OAM Principles": ~r/oam/,
      "Embedded Systems User's Guide": ~r/embedded/,
    ]
  ])
