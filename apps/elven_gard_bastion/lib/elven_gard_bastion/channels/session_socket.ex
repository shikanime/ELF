defmodule ElvenGardBastion.SessionSocket do
  use GenServer

  require Logger

  alias ElvenGardCitadel.Datastore.Account

  def start_worker(id) do
    with {:ok, pid} <-
           Swarm.register_name(
             id,
             ElvenGardBastion.SessionPool,
             :register,
             []
           ),
         :ok <- Swarm.join(:bastion_sessions, pid) do
      {:ok, pid}
    end
  end

  def get_worker(id) do
    case Swarm.whereis_name(id) do
      :undefined -> {:error, :session_not_found}
      session_pid -> {:ok, session_pid}
    end
  end

  def get_or_start_worker(id) do
    with {:ok, pid} <-
           Swarm.whereis_or_register_name(
             id,
             ElvenGardBastion.SessionPool,
             :register,
             []
           ),
         :ok <- Swarm.join(:bastion_sessions, pid) do
      {:ok, pid}
    end
  end

  def process(pid, msg) do
    GenServer.call(pid, {:process_packet, msg})
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    {:ok, args}
  end

  @impl true
  def handle_call({:process_packet, req}, from, state) do
    packet = state.crypto.decrypt(req)
    packet = GenServer.call({:process, state.protocol_pid}, packet)

    case Account.identify_user(packet.user_name, packet.user_password) do
      {:ok, user} ->
        res = %{
          user_name: user.name,
          client_id: packet.client_id,
          # TODO: Remove static server IP
          server_statuses: [
            %{
              ip: System.get_env("NODE_IP"),
              port: 4124,
              population: 0,
              # TODO: move to env
              population_limit: 200,
              world_id: 1,
              channel_id: 1,
              name: "Mainland"
            }
          ]
        }

        GenServer.reply(
          from,
          GenServer.call(
            state.protocol_pid,
            {:produce_packet, "loging_success.nsl", res}
          )
        )

        {:stop, :normal, state}

      {:error, reason} ->
        GenServer.reply(
          from,
          GenServer.call(
            state.protocol_pid,
            {:produce_packet, "bad_credential.nsl", %{}}
          )
        )

        Logger.warn(fn ->
          "An user failed to connect: #{inspect(reason)}"
        end)

        {:noreply, state}
    end
  end
end
