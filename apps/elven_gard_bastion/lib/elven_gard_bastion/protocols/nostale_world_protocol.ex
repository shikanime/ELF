defmodule ElvenGardBastion.NostaleWorldProtocol do
  @behaviour :ranch_protocol

  require Logger

  alias ElvenGardBastion.Network
  alias ElvenGardCitadel.Datastore.Account

  alias ElvenGardStdlib.{
    SessionCrypto,
    SessionPacket,
    WorldCrypto,
    UsernamePacket,
    PasswordPacket,
    CharacterSelectView,
    LoginView,
    PositionView,
    PasswordCrypto
  }

  def start_link(ref, socket, transporter, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transporter])}
  end

  def init(ref, socket, transporter) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transporter.setopts(socket, active: true) do
      {address, port} = Network.parse_peername(socket)

      :gen_server.enter_loop(__MODULE__, [], %{
        session_id: nil,
        stage: :validate_session,
        transporter: transporter,
        address: address,
        port: port
      })
    end
  end

  def handle_info({:tcp, _socket, raw_packet}, state = %{stage: :validate_session}) do
    decrypted_packet = SessionCrypto.decrypt(raw_packet)
    parsed_packet = SessionPacket.parse!(decrypted_packet)

    Logger.info(fn ->
      """
      Session packet received from #{state.address}:#{state.port} \
      of content: #{inspect(parsed_packet)}"\
      """
    end)

    {:noreply, %{state | session_id: parsed_packet.session_id, stage: :validate_credential}}
  end

  def handle_info({:tcp, socket, raw_packet}, state = %{stage: :validate_credential}) do
    decrypted_packets = WorldCrypto.decrypt(raw_packet, state.session_id)

    username_packet = UsernamePacket.parse!(Enum.at(decrypted_packets, 0))
    password_packet = PasswordPacket.parse!(Enum.at(decrypted_packets, 1))

    Logger.info(fn ->
      """
      Username packet received from #{state.address}:#{state.port} \
      of content: #{inspect(username_packet)}"\
      """
    end)

    Logger.info(fn ->
      """
      Password packet received from #{state.address}:#{state.port} \
      of content: #{inspect(password_packet)}"\
      """
    end)

    case Account.identify_user(username_packet.user_name, PasswordCrypto.encrypt(password_packet.user_password)) do
      {:ok, _user} ->
        response_packets =
          # TODO: Implement universe service
          CharacterSelectView.render("list_characters.nsl", %{
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

        Enum.each(
          response_packets,
          &state.transporter.send(
            socket,
            WorldCrypto.encrypt(&1)
          )
        )

        Logger.info(fn ->
          """
          Packet sent to #{state.address}:#{state.port} \
          of content: #{inspect(response_packets)}\
          """
        end)

        {:noreply, %{state | stage: :stash}}

      {:error, reason} ->
        response_packet = LoginView.render("bad_credential.nsl", %{})
        encrypted_packet = WorldCrypto.encrypt(response_packet)

        state.transporter.send(
          socket,
          encrypted_packet
        )

        Logger.warn(fn ->
          """
          Error raised from #{state.address}:#{state.port} \
          of content: #{inspect(reason)}\
          """
        end)

        {:noreply, state}
    end
  end

  def handle_info({:tcp, _socket, raw_packet}, state = %{stage: :stash}) do
    decrypted_packet = WorldCrypto.decrypt(raw_packet, state.session_id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(decrypted_packet)}"
    end)

    {:noreply, %{state | stage: :select_character}}
  end

  def handle_info({:tcp, socket, raw_packet}, state = %{stage: :select_character}) do
    decrypted_packet = WorldCrypto.decrypt(raw_packet, state.session_id)

    Logger.info(fn ->
      """
      Username packet received from #{state.address}:#{state.port} \
      of content: #{inspect(decrypted_packet)}"\
      """
    end)

    response_packet =
      PositionView.render("place_character.nsw", %{
        character_id: 1,
        map_name: "Nosville",
        position_x: 20,
        position_y: 20,
        music_id: 0
      })

    state.transporter.send(
      socket,
      WorldCrypto.encrypt(response_packet)
    )

    Logger.info(fn ->
      """
      Packet sent to #{state.address}:#{state.port} \
      of content: #{inspect(response_packet)}\
      """
    end)

    {:noreply, %{state | stage: :game}}
  end

  def handle_info({:tcp, _socket, raw_packet}, state = %{stage: :game}) do
    decrypted_packet = WorldCrypto.decrypt(raw_packet, state.session_id)

    Logger.warn(fn ->
      "Unimplemented packet: #{inspect(decrypted_packet)}"
    end)

    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state) do
    state.transporter.close(socket)
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
