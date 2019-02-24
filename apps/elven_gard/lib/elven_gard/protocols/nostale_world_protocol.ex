defmodule ElvenGard.NostaleWorldProtocol do
  @behaviour :ranch_protocol

  require Logger

  alias ElvenGard.{
    WorldCrypto,
    SessionCrypto,
    SessionRequest,
    LobbyResponse
  }
  alias ElvenGard.Endpoint.Client

  def start_link(ref, socket, transporter, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transporter])}
  end

  def init(ref, socket, transporter) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transporter.setopts(socket, active: true) do
      :gen_server.enter_loop(__MODULE__, [], %{
        id: "",
        step: :await_session,
        client: %Client{
          transporter: transporter
        }
      })
    end
  end

  def handle_info({:tcp, _socket, req}, state = %{step: :await_session}) do
    packet =
      req
      |> SessionCrypto.decrypt!()
      |> SessionRequest.parse!()

    Logger.info(fn ->
      "New session packet received: #{inspect(packet)}"
    end)

    {:noreply, %{state |
      id: packet.client_id,
      step: :await_username
    }}
  end

  def handle_info({:tcp, socket, req}, state = %{step: :await_username}) do
    packet = WorldCrypto.decrypt!(req, state.id)

    Logger.info(fn ->
      "New auth packet received: #{inspect(packet)} with state: #{inspect(state)}"
    end)

    case packet do
      [{_last_live, username}] ->
        # TODO: replace with real auth
        # ["username", username]
        {:noreply, state}

      [{_last_live, username}, {_last_live2, password}] ->
        # TODO: replace with real auth
        # [["username", username], ["password", password]]
        # TODO: replace with specific service
        Client.reply(state.client, socket, LobbyResponse.render("list_characters.nsl", %{
          characters: [
            %{
              name: "DarkyZ",
              slot: 1,
              gender: 1,
              hair_style: 1,
              hair_color: 1,
              class: 0,
              level: 30,
              job_level: 10,
              hero_level: 99,
              equipments: "-1.-1.-1.-1.-1.-1.-1.-1",
              pets: "-1"
            }
          ]
        }))
        {:noreply, state}
    end
  end

  def handle_info({:tcp, _socket, req}, state = %{step: :await_password}) do
    [{_last_live, password}] = WorldCrypto.decrypt!(req, state.id, true)
    # TODO: replace with real auth
    # ["password", password]
    {:noreply, state}
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
