defmodule ElvenGardBastion.NostaleLoginProtocol do
  @behaviour :ranch_protocol

  @client_hash Application.get_env(:elven_gard_bastion, :client_hash)
  @client_version Application.get_env(:elven_gard_bastion, :client_version)

  require Logger

  alias ElvenGardBastion.Network
  alias ElvenGardCitadel.Datastore.Account

  alias ElvenGardStdlib.{
    LoginCrypto,
    LoginPacket,
    LoginView
  }

  def start_link(ref, socket, transporter, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transporter])}
  end

  def init(ref, socket, transporter) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transporter.setopts(socket, active: true) do
      {address, port} = Network.parse_peername(socket)

      :gen_server.enter_loop(__MODULE__, [], %{
        transporter: transporter,
        address: address,
        port: port
      })
    end
  end

  def handle_info({:tcp, socket, raw_packet}, state) do
    decrypted_packet = LoginCrypto.decrypt(raw_packet)
    parsed_packet = LoginPacket.parse!(decrypted_packet)

    Logger.info(fn ->
      """
      Login packet received from #{state.address}:#{state.port} \
      of content: #{inspect(parsed_packet)}"\
      """
    end)

    with :ok <- check_client_version(parsed_packet),
         :ok <- check_client_client(parsed_packet),
         {:ok, user} <-
           Account.identify_user(
             parsed_packet.user_name,
             parsed_packet.user_password
           ) do
      response_packet =
        LoginView.render(
          "loging_success.nsl",
          %{
            user_name: user.name,
            session_id: parsed_packet.session_id,
            # TODO: Remove static server IP
            server_statuses: [
              %{
                ip: System.get_env("NODE_IP"),
                port: System.get_env("WORLD_PORT"),
                population: 0,
                # TODO: move to env
                population_limit: 200,
                world_id: 1,
                channel_id: 1,
                name: "Mainland"
              }
            ]
          }
        )

      state.transporter.send(
        socket,
        LoginCrypto.encrypt(response_packet)
      )

      Logger.info(fn ->
        """
        Packet sent to #{state.address}:#{state.port} \
        of content: #{inspect(response_packet)}\
        """
      end)

      {:stop, :normal, state}
    else
      {:error, reason} ->
        response_packet = LoginView.render("bad_credential.nsl", %{})

        state.transporter.send(
          socket,
          LoginCrypto.encrypt(response_packet)
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

  defp check_client_version(packet) do
    if packet.client_version == @client_version do
      :ok
    else
      {:error, :client_outdated}
    end
  end

  defp check_client_client(packet) do
    expected_hash = :crypto.hash(:md5, @client_hash <> packet.user_name) |> Base.encode16()
    |> IO.inspect
    if expected_hash == packet.client_hash do
      :ok
    else
      {:error, :client_outdated}
    end
  end
end
