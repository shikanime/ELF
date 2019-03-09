defmodule ElvenGardBastion.Protocol do
  @behaviour :ranch_protocol

  use GenStateMachine

  @client_hash Application.get_env(:elven_gard_bastion, :client_hash)
  @client_version Application.get_env(:elven_gard_bastion, :client_version)

  require Logger

  alias ElvenGardGuard.{
    Account,
    SessionSocket
  }

  alias ElvenGardLib.{
    SignInCrypto,
    SignInPacket,
    AuthentificationView
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
      conn =  {socket, transport}
      :gen_statem.enter_loop(__MODULE__, [], :connect, %{
        crypto: SignInCrypto,
        conn: conn
      })
    end
  end

  @impl true
  def handle_event(:info, {:tcp, _socket, packet}, :connect, data) do
    decrypted_packet = SignInCrypto.decrypt(packet)
    sign_in_packet = SignInPacket.parse(decrypted_packet)

    case connect_user(sign_in_packet) do
      {:ok, params} ->
        reply(data.conn, data.crypto, AuthentificationView, :sign_in, params)
        {:stop, :normal, data}

      {:error, reason} ->
        reply(data.conn, data.crypto, AuthentificationView, reason, %{})
        {:stop, reason}
    end
  end

  def connect_user(packet) do
    with :ok              <- validate_client(packet),
         {:ok, user}      <- authenticate_user(packet),
         {:ok, client_id} <- claim_slot(),
         {:ok, worlds}    <- list_worlds() do
      {:ok, %{
        user_name: user.name,
        client_id: client_id,
        worlds: worlds
      }}
    end
  end

  defp validate_client(packet) do
    with :ok <- validate_client_version(packet),
         :ok <- validate_client_hash(packet),
         do: :ok
  end

  defp validate_client_version(packet) do
    if packet.client_version == @client_version do
      :ok
    else
      {:error, :outdated_client}
    end
  end

  defp validate_client_hash(packet) do
    expected_hash =
      :crypto.hash(:md5, @client_hash <> packet.user_name)
      |> Base.encode16()

    if expected_hash == packet.client_hash do
      :ok
    else
      Logger.warn(fn ->
        "Client hash: #{packet.client_hash} is different from expected hash: ${expected_hash}"
      end)
      {:error, :corrupted_client}
    end
  end

  defp available_slot() do
    client_id = :random.uniform(2_147_483_647)
    session_id = UUID.uuid5(nil, client_id |> to_string())

    case Swarm.whereis_name(session_id) do
      :undefined ->
        {client_id, session_id}

      _ ->
        available_slot()
    end
  end

  def claim_slot() do
    {client_id, session_id} = available_slot()

    with :ok <- SessionSocket.start_worker(session_id),
         do: {:ok, client_id}
  end

  def authenticate_user(packet) do
    Account.authenticate_user(packet.user_name, packet.user_password_hash)
  end

  def list_worlds() do
    worlds = Swarm.multi_call(:universe_worlds, :get_status)
    |> Enum.map(&(format_world(&1)))
    {:ok, worlds}
  end

  defp format_world(world) do
    Map.new(world, fn
      {:id, id} -> {:world_id, id}
      x -> x
    end)
    # TODO: remove this channel placeholder
    |> Map.put(:channel_id, 0)
  end

  def reply(conn, crypto, view, name, packet) do
    res_packets = view.render(name, packet)

    if is_list(res_packets) do
      Enum.each(res_packets, &(reply(conn, crypto, &1)))
    else
      reply(conn, crypto, res_packets)
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
