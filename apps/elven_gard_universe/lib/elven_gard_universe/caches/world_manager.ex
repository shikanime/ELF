defmodule ElvenGardUniverse.WorldManager do
  use GenServer

  require Logger

  def start_worker(id, attrs \\ %{}) do
    with {:ok, pid} <-
           Swarm.register_name(
             id,
             ElvenGardUniverse.Swarm,
             :register,
             [__MODULE__, attrs]
           ),
         :ok <- Swarm.join(:universe_worlds, pid) do
      :ok
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
             ElvenGardUniverse.Swarm,
             :register,
             []
           ),
         :ok <- Swarm.join(:universe_worlds, pid) do
      {:ok, pid}
    end
  end

  def start_link(init_args) do
    GenServer.start_link(__MODULE__, [init_args])
  end

  @impl true
  def init(init_args) do
    {:ok, %{
      id: init_args.id,
      name: init_args.name,
      ip: init_args.ip,
      port: init_args.port
    }}
  end

  @impl true
  def handle_call(:get_status, _from, state) do
    {:reply, state, state}
  end
end
