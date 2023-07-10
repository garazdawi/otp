a = [
	proglang: :erlang,
        source_url: "https://github.com/erlang/otp",
        source_ref: "master",
        extras: Path.wildcard("lib/#{System.get_env("APP")}/doc/src/*.md"),
	deps: (Path.wildcard("lib/*/ebin") |> Enum.map(
              fn path -> app = Path.split(path) |> Enum.at(1);
              {String.to_atom(app), Path.join(["..",app])} end)) ++ [erts: "docs/erts"]
]
IO.inspect a
a
