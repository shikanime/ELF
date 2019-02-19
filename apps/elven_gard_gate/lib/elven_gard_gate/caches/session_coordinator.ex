defmodule ElvenGardGate.SessionCoordinator do
  use GenStateMachine

  def start_coordinator() do
    id = UUID.uuid4()
    with :ok <- register_coordinator(id),
         do: id
  end

  def register_coordinator(id) do
    with {:ok, pid} <- Swarm.register_name(id, ElvenGardGate.SessionSuppervisor, :register, [id]),
          :ok       <- Swarm.join(:eg_gate_sessions, pid),
          do: :ok
  end

  def init(_args) do
    {:ok, :checkout, %{
      packet: ElvenGardGate.LoginRequest,
      crypto: ElvenGardGate.LoginCrypto
    }}
  end
end
