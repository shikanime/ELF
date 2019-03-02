defmodule ElvenGardCitadel.WorldProtocol do
  @behaviour :ranch_protocol

  use GenStateMachine

  require Logger

  alias ElvenGardGuard.Account
  alias ElvenGardCitadel.{
    Utils,
    SessionCrypto,
    WorldCrypto,
    ClientAuthPacket,
    UsernamePacket,
    PasswordPacket,
    LobbyView,
    AuthentificationView,
    HeroView
  }

  @impl true
  def start_link(ref, socket, transport, _opts) do
    args = [{ref, socket, transport}]
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, args)}
  end

  @impl true
  def init({ref, socket, transport}) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transport.setopts(socket, active: true) do
      {address, port} = Utils.parse_peername(socket)

      :gen_statem.enter_loop(__MODULE__, [], :connect_client, %{
        session_id: nil,
        client_id: nil,
        packet_id: nil,
        address: address,
        port: port,
        connection: {socket, transport, WorldCrypto},
      })
    end
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :connect_client, data) do
    packet = SessionCrypto.decrypt(packet)
    packet = ClientAuthPacket.parse(packet)
    {:next_state, :validate_credential, %{
      data |
      client_id: packet.client_id,
      session_id: packet.session_id
    }}
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :validate_credential, data) do
    packets = WorldCrypto.decrypt(packet, data.client_id)

    {username_packet_id, username_packet} = Enum.at(packets, 0)
    {password_packet_id, password_packet} = Enum.at(packets, 1)

    if lost_packet?(username_packet_id, password_packet_id) do
      Logger.error(fn ->
        """
        A packet have been lost from #{data.address}:#{data.port} \
        from packet #{inspect(username_packet_id)} to packet #{inspect(password_packet_id)} "\
        """
      end)
    end

    username_packet = UsernamePacket.parse(username_packet)
    password_packet = PasswordPacket.parse(password_packet)

    case Account.authenticate_user(username_packet.user_name, password_packet.user_password_hash) do
      {:ok, _user} ->
        Utils.send(
          data.connection,
          LobbyView,
          "list_heros", %{
            heros: [
              %{
                name: "PlayerZ",
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
          }
        )
        {:next_state, :ignore_stash, %{data | packet_id: password_packet_id}}

      {:error, reason} ->
        Utils.send(data.connection, AuthentificationView, "unvalid_credential", %{})
        {:shutdown, reason, data}
    end
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :ignore_stash, data) do
    packet = WorldCrypto.decrypt(packet, data.client_id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(packet)}"
    end)

    {:next_state, :select_hero, data}
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :select_hero, data) do
    _packet = WorldCrypto.decrypt(packet, data.client_id)

    # TODO: replace placeholder
    Utils.send(data.connection, HeroView, "spawn_hero", %{
      id: 1,
      name: "Player",
      class: 1,
      gender: 1,
      hair_style: 1,
      hair_color: 1,
      group_id: "-1",
      family_id: "-1",
      family_name: "-",
      authority: 0,
      dignity: 16,
      compliment: 0,
      morph: 0,
      invisible: false,
      family_level: 0,
      sp_upgrade: 0,
      arena_winner: 0,
    })

    Utils.send(data.connection, HeroView, "move_hero", %{
      id: 1,
      map_name: "Nosville",
      position_x: :rand.uniform(6) + 76,
      position_y: :rand.uniform(5) + 113,
      music_id: 0
    })

    {:next_state, :in_world, data}
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :in_world, data) do
    packet = WorldCrypto.decrypt(packet, data.client_id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(packet)}"
    end)

    :keep_state_and_data
  end

  def lost_packet?(curr_packet, inc_packet) do
    curr_packet + 1 == inc_packet
  end
end
