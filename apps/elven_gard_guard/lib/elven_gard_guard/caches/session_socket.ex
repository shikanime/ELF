defmodule ElvenGardGuard.SessionSocket do
  use GenServer

  require Logger

  def start_worker(id) do
    with {:ok, pid} <-
           Swarm.register_name(
             id,
             ElvenGardGuard.SessionPool,
             :register,
             []
           ),
         :ok <- Swarm.join(:bastion_sessions, pid) do
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
             ElvenGardGuard.SessionPool,
             :register,
             []
           ),
         :ok <- Swarm.join(:bastion_sessions, pid) do
      {:ok, pid}
    end
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    {:ok, args}
  end
end
