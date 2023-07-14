{global,_} = Code.eval_file Path.join(System.get_env("ERL_TOP"),"ex_doc.exs");
Keyword.merge(global,
  [
    extras: Path.wildcard("system/doc/{top,general_info}/*.md") ++
    Path.wildcard("system/doc/top/*/*.md"),
    main: "readme",
    api_reference: false,
    groups_for_extras: [ "System Documentation": ~r{top/system},
                         "Basic": ~r/basic/,
                         "Database": ~r/database/,
                         "Operations & Mainteinance": ~r/oam/,
                         "Interfaces & Communication": ~r/interfaces/,
                         "Tools": ~r/tools/,
                         "Test": ~r/test/,
                         "Documentation": ~r/docs/
                       ],
    deps: (Path.wildcard("lib/*/ebin") |> Enum.map(
    fn path -> app = Path.split(path) |> Enum.at(1);
      {String.to_atom(app), Path.join([app])} end)) ++ [erts: "erts", system: "system"]
  ]) |> IO.inspect
