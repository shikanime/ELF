defmodule ElvenGard.NostaleLoginProtocol do
  @behaviour :ranch_protocol

  alias ElvenGard.{
    LoginCrypto,
    LoginRequest,
    LoginResponse
  }
  alias ElvenGardTower.AccountRepo
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

    case AccountRepo.identify_user(packet.user_name, packet.user_password) do
      {:ok, user} ->
        Client.reply(
          state.client,
          socket,
          LoginResponse.render("loging_success.nsl", %{
            user_id:        user.id,
            session_id:     "",
            server_status:  []
          })
        )
        {:stop, :normal, state}
      {:error, _reason} ->
        Client.reply(
          state.client,
          socket,
          LoginResponse.render("bad_credential.nsl", %{})
        )
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
