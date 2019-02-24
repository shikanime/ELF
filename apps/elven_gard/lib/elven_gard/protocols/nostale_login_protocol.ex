defmodule ElvenGard.NostaleLoginProtocol do
  @behaviour :ranch_protocol

  require Logger

  alias ElvenGard.{
    LoginCrypto,
    LoginRequest,
    LoginResponse
  }
  alias ElvenGardTower.Datastore.Account
  alias ElvenGard.Endpoint.Client

  def start_link(ref, socket, transporter, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transporter])}
  end

  def init(ref, socket, transporter) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transporter.setopts(socket, active: true) do
      :gen_server.enter_loop(__MODULE__, [], %{
        client: %Client{
          transporter: transporter
        }
      })
    end
  end

  def handle_info({:tcp, socket, req}, state) do
    packet =
      req
      |> LoginCrypto.decrypt!()
      |> LoginRequest.parse!()

    Logger.info(fn ->
      "New packet received: #{inspect(packet)}"
    end)

    case Account.identify_user(packet.user_name, packet.user_password) do
      {:ok, user} ->
        response = LoginResponse.render("loging_success.nsl", %{
          user_name:        user.name,
          client_id:        packet.client_id,
          # TODO: Remove static server IP
          server_statuses:  [%{
            ip: System.get_env("NODE_IP"),
            port: 4124,
            population: 0,
            world_id: 1,
            channel_id: 1,
            name: "Mainland"
          }]
        })


        Client.reply(
          state.client,
          socket,
          LoginCrypto.encrypt!(response)
        )

        Logger.info(fn ->
          "#{user.name} have been connected with packet: #{response}"
        end)

        {:stop, :normal, state}
      {:error, reason} ->
        response = LoginResponse.render("bad_credential.nsl", %{})

        Client.reply(
          state.client,
          socket,
          LoginCrypto.encrypt!(response)
        )

        Logger.warn(fn ->
          "An user failed to connect: #{inspect(reason)}"
        end)

        {:noreply, state}
    end
  end

  def handle_info({:tcp_closed, socket}, state) do
    Client.close(state.client, socket)
    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _socket, reason}, state) do
    case reason do
      :closed -> {:stop, :normal, state}
      :timeout -> {:stop, :normal, state}
      reason -> {:stop, reason, state}
    end
  end
end
