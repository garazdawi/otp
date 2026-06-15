set +e
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq >/dev/null 2>&1
apt-get install -y -qq curl unzip ca-certificates postgresql sudo libssl3 libncurses6 procps git >/dev/null 2>&1
export ERL_TOP=/otp PATH=/otp/bin:/elixir/bin:$PATH
export LANG=C.UTF-8 LC_ALL=C.UTF-8 ELIXIR_ERL_OPTIONS="+fnu"
echo "built OTP: $(/otp/bin/erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().')"
# --- postgres ---
service postgresql start >/dev/null 2>&1
sleep 3
sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'postgres';" >/dev/null 2>&1
sudo -u postgres psql -c "CREATE DATABASE bench;" >/dev/null 2>&1
sudo -u postgres psql -d bench -c "CREATE TABLE items (id bigserial primary key, name text, qty int, price int);" >/dev/null 2>&1
sudo -u postgres psql -d bench -c "INSERT INTO items (name,qty,price) SELECT 'seed'||g, g%10, g FROM generate_series(1,2000) g;" >/dev/null 2>&1
echo "postgres ready: $(sudo -u postgres psql -d bench -tAc 'select count(*) from items;')"
# --- elixir ---
curl -sL -o /ex.zip https://github.com/elixir-lang/elixir/releases/download/v1.19.2/elixir-otp-28.zip
mkdir -p /elixir && unzip -qo /ex.zip -d /elixir
# --- HTTP load driver (Erlang) ---
cat > /tmp/http_db.erl <<'HDB'
-module(http_db).
-export([run/3]).
run(N, DurMs, _) ->
    Items = lists:join($,, [io_lib:format("{\"name\":\"item~b\",\"qty\":~b,\"price\":~b}", [I, I rem 7 + 1, I*3]) || I <- lists:seq(1,5)]),
    Create = req(iolist_to_binary(["{\"op\":\"create\",\"items\":[", Items, "]}"])),
    List = req(<<"{\"op\":\"list\"}">>),
    Batch = iolist_to_binary([Create, List, Create, List, Create, List, Create, List]),
    Parent = self(),
    Ws = [spawn_link(fun() -> w(Batch, 0, Parent) end) || _ <- lists:seq(1, N)],
    timer:sleep(DurMs),
    [W ! stop || W <- Ws],
    T = lists:sum([receive {done, C} -> C end || _ <- Ws]),
    io:format("responses: ~b (~.1f k/s)~n", [T, T*1000/DurMs/1000]).
w(Batch, C, Parent) ->
    {ok, S} = gen_tcp:connect("127.0.0.1", 4001, [binary, {active, false}, {nodelay, true}]),
    w(S, Batch, C, Parent).
w(S, Batch, C, Parent) ->
    receive stop -> Parent ! {done, C}, gen_tcp:close(S)
    after 0 ->
        ok = gen_tcp:send(S, Batch),
        M = drain(S, 8, 0),
        w(S, Batch, C + M, Parent)
    end.
drain(_S, 0, Acc) -> Acc;
drain(S, Left, Acc) ->
    {ok, D} = gen_tcp:recv(S, 0, 15000),
    M = length(binary:matches(D, <<"HTTP/1.1 200">>)),
    drain(S, Left - M, Acc + M).
req(Body) ->
    iolist_to_binary(["POST /api HTTP/1.1\r\nhost: localhost\r\ncontent-type: application/json\r\ncontent-length: ",
                      integer_to_list(byte_size(Body)), "\r\n\r\n", Body]).
HDB
/otp/bin/erlc -o /tmp /tmp/http_db.erl 2>/dev/null
# --- Ash + Bandit server ---
cat > /server.exs <<'EXS'
Mix.install([
  {:bandit, "~> 1.5"},
  {:jason, "~> 1.4"},
  {:ash, "~> 3.0"},
  {:ash_postgres, "~> 2.0"}
])
Logger.configure(level: :error)

Application.put_env(:bench, Bench.Repo,
  hostname: "localhost", username: "postgres", password: "postgres",
  database: "bench", pool_size: 10)

defmodule Bench.Repo do
  use AshPostgres.Repo, otp_app: :bench
  def installed_extensions, do: []
  def min_pg_version, do: %Version{major: 14, minor: 0, patch: 0}
end

defmodule Bench.Item do
  use Ash.Resource,
    domain: Bench.Domain,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "items"
    repo Bench.Repo
  end

  attributes do
    integer_primary_key :id
    attribute :name, :string, public?: true
    attribute :qty, :integer, public?: true
    attribute :price, :integer, public?: true
  end

  actions do
    defaults [:read, :create]
    default_accept [:name, :qty, :price]
  end
end

defmodule Bench.Domain do
  use Ash.Domain
  resources do
    resource Bench.Item
  end
end

{:ok, _} = Bench.Repo.start_link()

defmodule Api do
  import Plug.Conn
  def init(o), do: o
  def call(conn, _) do
    {:ok, body, conn} = read_body(conn)
    req = Jason.decode!(body)
    resp =
      case req["op"] do
        "create" ->
          n =
            Enum.reduce(req["items"], 0, fn it, acc ->
              Bench.Item
              |> Ash.Changeset.for_create(:create, %{
                "name" => it["name"], "qty" => it["qty"], "price" => it["price"]
              })
              |> Ash.create!()
              acc + 1
            end)
          %{"inserted" => n}
        "list" ->
          items =
            Bench.Item
            |> Ash.Query.sort(id: :desc)
            |> Ash.Query.limit(20)
            |> Ash.read!()
            |> Enum.map(fn i -> %{"id" => i.id, "name" => i.name, "qty" => i.qty, "price" => i.price} end)
          %{"items" => items, "count" => length(items)}
        _ -> %{"echo" => true}
      end
    conn |> put_resp_content_type("application/json") |> send_resp(200, Jason.encode!(resp))
  end
end

{:ok, _} = Application.ensure_all_started(:bandit)
{:ok, _} = Bandit.start_link(plug: Api, port: 4001, ip: {127, 0, 0, 1})
IO.puts("server up")
:erts_debug.set_internal_state(:available_internal_state, true)
:timer.sleep(8000)
:erts_debug.set_internal_state(:alloc_profile_all, true)
:erts_debug.set_internal_state(:alloc_profile_sites, true)
:timer.sleep(12000)
:erts_debug.set_internal_state(:alloc_profile_all, false)
sites = :erts_debug.get_internal_state(:alloc_profile_sites)
total = Enum.reduce(sites, 0, fn {_, w}, a -> a + w end)
top = sites |> Enum.sort_by(fn {_, w} -> -w end) |> Enum.take(40)
IO.puts("GALLOC_TOTAL #{total} words (#{Float.round(total*8/1.0e6,1)} MB)")
for {mfa, w} <- top, do: IO.puts("GALLOC #{Float.round(100*w/max(1,total),2)}% #{inspect(mfa)} #{w}")
System.halt(0)
EXS
ERL_ALLOC_PROFILE=1 elixir /server.exs > /srv.out 2>&1 &
for i in $(seq 1 200); do grep -q "server up" /srv.out 2>/dev/null && break; sleep 2; done
grep -q "server up" /srv.out || { echo "SERVER FAILED"; tail -40 /srv.out; exit 42; }
ERL_ALLOC_PROFILE=1 /otp/bin/erl -noshell -pa /tmp -eval '(catch http_db:run(8, 28000, x)), halt().' > /load.out 2>&1 &
for i in $(seq 1 60); do grep -q "GALLOC_TOTAL" /srv.out 2>/dev/null && break; sleep 1; done
echo "=== load ==="; grep -oE "responses: .*" /load.out | head -1
echo "=== allocation worklist (Bandit + Ash + Postgres) ==="
grep -E "GALLOC" /srv.out
