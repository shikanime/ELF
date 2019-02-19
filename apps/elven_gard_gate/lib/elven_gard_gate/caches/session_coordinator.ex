defmodule ElvenGardGate.SessionCoordinator do
  use GenServer

  def start_coordinator() do
    UUID.uuid4()
    |> register_coordinator()
  end

  def register_coordinator(id) do
    with {:ok, pid} <- Swarm.register_name(id, ElvenGardGate.SessionSuppervisor, :register, [id]),
          :ok       <- Swarm.join(:eg_gate_sessions, pid),
          do: {:ok, id}
  end

  def init(args) do
    {:ok, args}
  end
end
