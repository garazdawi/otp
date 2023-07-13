[
	proglang: :erlang,
        source_url: "https://github.com/erlang/otp",
        source_ref: "master",
        extras: ["system/doc/README.md" | Path.wildcard("system/doc/*/*.md")],
        main: "readme",
        groups_for_extras: [ "General Information": ~r/general_info/,
                             "Installation Guide": ~r/installation_guide/,
                             "System Principles": ~r/system_principles/,
                             "Embedded Systems User's Guide": ~r/embedded/,
                             "Getting Started With Erlang": ~r/getting_started/,
                             "Erlang Reference Manual": ~r/reference_manual/,
                             "Programming Examples": ~r/programming_examples/,
                             "Efficiency Guide": ~r/efficiency_guide/,
                             "Interoperability Tutorial": ~r/tutorial/,
                             "OTP Design Principles": ~r/design_principles/,
                             "OAM Principles": ~r/oam/],
	deps: (Path.wildcard("lib/*/ebin") |> Enum.map(
              fn path -> app = Path.split(path) |> Enum.at(1);
              {String.to_atom(app), Path.join(["..",app])} end)) ++ [erts: "../erts"]
]
