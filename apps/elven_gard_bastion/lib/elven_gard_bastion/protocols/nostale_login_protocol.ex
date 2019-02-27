defmodule ElvenGardBastion.NostaleLoginProtocol do
  @behaviour :ranch_protocol
  @behaviour :gen_statem

  @client_hash Application.get_env(:elven_gard_bastion, :client_hash)
  @client_version Application.get_env(:elven_gard_bastion, :client_version)

  require Logger

  alias ElvenGardBastion.{
    AccountRepo,
    NetworkHelpers,
    SessionSocket
  }

  alias ElvenGardStdlib.{
    LoginCrypto,
    LoginPacket,
    LoginView
  }

  def start_link(ref, socket, transport, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, socket, transport}])}
  end

  def init({ref, socket, transport}) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transport.setopts(socket, active: true) do
      {address, port} = NetworkHelpers.parse_peername(socket)

      :gen_statem.enter_loop(__MODULE__, [], :connect_client, %{
        address: address,
        port: port,
        connection: {socket, transport, LoginCrypto}
      })
    end
  end

  def callback_mode() do
    :state_functions
  end

  def connect_client(:info, {:tcp, _socket, packet}, data) do
    packet = LoginCrypto.decrypt(packet)
    packet = LoginPacket.parse!(packet)

    if valid_client?(packet) do
      case login_user(packet) do
        {:ok, {{_session_id, client_id}, user}} ->
          NetworkHelpers.send(data.connection, LoginView, "loging_success.nsl", %{
            user_name: user.name,
            client_id: client_id,
            # TODO: Remove static server IP
            server_statuses: [
              %{
                ip: System.get_env("NODE_IP"),
                port: System.get_env("ELVEN_WORLD_PORT"),
                population: 0,
                # TODO: move to env
                population_limit: 200,
                world_id: 1,
                channel_id: 1,
                name: "Mainland"
              }
            ]
          })
          {:stop, :normal, data}

        {:error, _reason} ->
          NetworkHelpers.send(data.connection, LoginView, "cant_login.nsl", %{})
          :keep_state_data
      end
    else
      NetworkHelpers.send(data.connection, LoginView, "bad_credential.nsl", %{})
      :keep_state_data
    end
  end

  defp valid_client?(packet) do
    valid_client_version?(packet) and valid_client_hash?(packet)
  end

  defp valid_client_version?(packet) do
    packet.client_version == @client_version
  end

  defp valid_client_hash?(packet) do
    expected_hash =
      :crypto.hash(:md5, @client_hash <> packet.user_name)
      |> Base.encode16()

    expected_hash == packet.client_hash
  end

  def generate_identity() do
    client_id = :random.uniform(2_147_483_647)
    session_id = UUID.uuid5(nil, client_id |> to_string())

    case Swarm.whereis_name(session_id) do
      :undefined ->
        {client_id, session_id}

      _ ->
        generate_identity()
    end
  end

  def login_user(packet) do
    with {:ok, user} <- AccountRepo.identify_user(packet.user_name, packet.user_password) do
      identity = generate_identity()
      notify_user(identity)
      {:ok, {identity, user}}
    end
  end

  def notify_user({_, session_id}) do
    case SessionSocket.start_worker(session_id) do
      {:ok, _session_id} ->
        :ok

      {:error, _reason} = error ->
        error
    end
  end
end
