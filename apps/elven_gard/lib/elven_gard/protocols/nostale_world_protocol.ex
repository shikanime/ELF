defmodule ElvenGard.NostaleWorldProtocol do
  @behaviour :ranch_protocol

  require Logger

  alias ElvenGard.{
    WorldCrypto,
    SessionCrypto,
    SessionRequest,
    UsernameRequest,
    LobbyResponse,
    PasswordRequest,
    PositionResponse
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
      id: packet.session_id,
      step: :await_username
    }}
  end

  def handle_info({:tcp, socket, req}, state = %{step: :await_username}) do
    packets = WorldCrypto.decrypt!(req, state.id)

    username_packet = UsernameRequest.parse!(Enum.at(packets, 0))
    password_packet = PasswordRequest.parse!(Enum.at(packets, 1))

    # TODO: handle via an event loop
    Logger.info(fn ->
      "New auth packet received: #{inspect(username_packet)} with state: #{inspect(state)}"
    end)

    Logger.info(fn ->
      "New auth packet received: #{inspect(password_packet)} with state: #{inspect(state)}"
    end)

    # TODO: handle packet id
    new_state = state
    |> Map.put(:username, username_packet.data.user_name)
    |> Map.put(:password, password_packet.data.password)

    responses = LobbyResponse.render("list_characters.nsl", %{
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
    })

    Enum.each(responses, &(Client.reply(state.client, socket, WorldCrypto.encrypt!(&1))))

    Logger.info(fn ->
      "#{username_packet.data.user_name} have been connected with packet: #{inspect(responses)}"
    end)

    {:noreply, %{new_state |
      step: :await_select_character
    }}
  end

  # def handle_info({:tcp, socket, req}, state = %{step: :await_select_character}) do
  #   packet = WorldCrypto.decrypt!(req, state.id)
  #   |> Enum.at(0)
  #   |> String.split()

  #   Logger.warn(fn ->
  #     "New select packet: #{inspect(packet)}"
  #   end)

  #   if Enum.at(packet, 1) === "select" do
  #     response = PositionResponse.render("place_character.nsw", %{
  #       character_id: 1,
  #       map_name: "Nosville",
  #       position_x: 20,
  #       position_y: 20,
  #       music_id: 0
  #     })

  #     Client.reply(state.client, socket, WorldCrypto.encrypt!(response))

  #     Logger.info(fn ->
  #       "An user spawn with: #{inspect(response)}"
  #     end)

  #     {:noreply, %{state |
  #       step: :await_game_command
  #     }}
  #   end

  #   {:noreply, state}
  # end

  def handle_info({:tcp, _socket, req}, state) do
    packet = WorldCrypto.decrypt!(req, state.id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(packet)}"
    end)

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
