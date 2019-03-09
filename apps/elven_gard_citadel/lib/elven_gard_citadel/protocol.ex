defmodule ElvenGardCitadel.Protocol do
  @behaviour :ranch_protocol

  use GenStateMachine

  require Logger

  alias ElvenGardGuard.{
    Account,
    SessionSocket
  }

  alias ElvenGardCitadel.{

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
      :gen_statem.enter_loop(__MODULE__, [], :connect, %{
        session_id: nil,
        client_id: nil,
        packet_id: nil,
        crypto: WorldCrypto,
        conn: {socket, transport},
      })
    end
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :connect, data) do
    decryted_packets = SessionCrypto.decrypt(packet)
    client_auth_packet = ClientAuthPacket.parse(decryted_packets)

    if slot_claimed?(client_auth_packet) do
      {:next_state, :validate_credential, %{
        data |
        client_id: client_auth_packet.client_id,
        session_id: client_auth_packet.session_id
      }}
    else
      reply(data.conn, data.crypto, AuthentificationView, "session_unclaimed", %{})
      {:error, :session_unclaimed}
    end
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :validate_credential, data) do
    decryted_packets = WorldCrypto.decrypt(packet, data.client_id)

    {username_packet_id, username_packet} = Enum.at(decryted_packets, 0)
    {password_packet_id, password_packet} = Enum.at(decryted_packets, 1)

    if lost_packet?(username_packet_id, password_packet_id) do
      Logger.error(fn ->
        """
        A packet have been lost from client: #{data.client_id} of session: #{data.session_id} \
        from packet #{inspect(username_packet_id)} to packet #{inspect(password_packet_id)} "\
        """
      end)
    end

    username_packet = UsernamePacket.parse(username_packet)
    password_packet = PasswordPacket.parse(password_packet)

    case Account.authenticate_user(username_packet.user_name, password_packet.user_password_hash) do
      {:ok, _user} ->
        params = %{
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
        reply(data.conn, data.crypto, LobbyView, "list_heros", params)
        {:next_state, :ignore_stash, %{data | packet_id: password_packet_id}}

      {:error, reason} ->
        reply(data.conn, data.crypto, AuthentificationView, "unvalid_credential", %{})
        {:shutdown, reason, data}
    end
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :ignore_stash, data) do
    decryted_packet = WorldCrypto.decrypt(packet, data.client_id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(decryted_packet)}"
    end)

    {:next_state, :select_hero, data}
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :select_hero, data) do
    _decryted_packet = WorldCrypto.decrypt(packet, data.client_id)

    # TODO: replace placeholder
    reply(data.conn, data.crypto, HeroView, "sync_hero", %{
      hero_id: 1,
      hero_name: "Player",
      hero_class: :neutre,
      hero_gender: :female,
      hero_hair_style: :b,
      hero_hair_color: :nutmeg,
      hero_sp_upgrade?: true,
      hero_arena_winner?: true,
      hero_invisible?: false,
      hero_morph: 0,
      family_id: "-1",
      family_name: "-",
      family_level: 0,
      group_id: "-1",
      authority: 0,
      dignity: 16,
      compliment: 0,
    })

    reply(data.conn, data.crypto, HeroView, "move_hero", %{
      hero_id: 1,
      hero_position_x: :rand.uniform(6) + 76,
      hero_position_y: :rand.uniform(5) + 113,
      map_name: "Nosville",
      map_music_id: 0
    })

    {:next_state, :in_world, data}
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :in_world, data) do
    decryted_packet = WorldCrypto.decrypt(packet, data.client_id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(decryted_packet)}"
    end)

    :keep_state_and_data
  end

  def lost_packet?(curr_packet, inc_packet) do
    curr_packet + 1 !== inc_packet
  end

  def slot_claimed?(packet) do
    # TODO: reverse Nostale error packets
    case SessionSocket.get_worker(packet.session_id) do
      {:ok, _session_pid} ->
        true

      {:error, _reason} ->
        false
    end
  end

  def reply(conn, crypto, view, name, packet) do
    encrypted_packets = view.render(name, packet)

    if is_list(encrypted_packets) do
      Enum.each(encrypted_packets, &(reply(conn, crypto, &1)))
    else
      reply(conn, crypto, encrypted_packets)
    end

    :ok
  end

  defp reply({socket, transport}, crypto, packet) do
    encrypted_packet = crypto.encrypt(packet)
    transport.send(socket, encrypted_packet)
    Logger.info(fn ->
      "packet sent: #{packet}"
    end)
  end
end
