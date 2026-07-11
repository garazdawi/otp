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

    resp = %{
      "user" => Map.put(user, "active", true),
      "total" => total,
      "names" => names,
      "count" => length(items)
    }

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
# let the driver connect + warm
:timer.sleep(4000)
# reset
:erts_debug.set_internal_state(:recv_stats, true)
# measured window
:timer.sleep(12000)
stats = :erts_debug.get_internal_state(:recv_stats)
IO.puts("RECV_STATS " <> inspect(stats))
# outlive the driver so it can print
:timer.sleep(12000)
System.halt(0)
