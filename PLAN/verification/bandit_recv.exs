# T2FULL M0.R -- Bandit/Plug JSON API, measures receive-path classification
# on the connection-handler processes under pipelined HTTP load.
Mix.install([
  {:bandit, "~> 1.5"},
  {:jason, "~> 1.4"},
  {:plug, "~> 1.16"}
])

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

:erts_debug.set_internal_state(:available_internal_state, true)
:timer.sleep(4000)                            # let the driver connect + warm
:erts_debug.set_internal_state(:recv_stats, true)   # reset
:timer.sleep(12000)                           # measured window
stats = :erts_debug.get_internal_state(:recv_stats)
IO.puts("RECV_STATS " <> inspect(stats))
:timer.sleep(12000)                           # outlive the driver so it can print
System.halt(0)
