deps = ~w(bandit thousand_island plug plug_crypto telemetry websock_adapter websock hpax mime jason)
for d <- deps do
  p = "/tmp/census_phx/_build/dev/lib/#{d}/ebin"
  if File.dir?(p), do: Code.prepend_path(p)
end
defmodule Api do
  import Plug.Conn
  def init(o), do: o
  def call(conn, _) do
    {:ok, body, conn} = read_body(conn)
    %{"items" => items, "user" => user} = Jason.decode!(body)
    total = Enum.reduce(items, 0, fn %{"qty" => q, "price" => p}, acc -> acc + q * p end)
    names = Enum.map(items, fn %{"name" => n} -> String.upcase(n) end)
    resp = %{"user" => Map.put(user, "active", true), "total" => total,
             "names" => names, "count" => length(items)}
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(resp))
  end
end
Logger.configure(level: :error)
{:ok, _} = Application.ensure_all_started(:bandit)
{:ok, _} = Bandit.start_link(plug: Api, port: 4001, ip: {127, 0, 0, 1})
IO.puts("server up")
:timer.sleep(8000)
:code.add_patha(~c"/tmp/prof29")
:t2_prof.run_to_file(15000, 5, ~c"/tmp/prof/bandit.report")
:timer.sleep(20000)
