defmodule ElvenGardGate.SessionSuppervisor do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      {ElvenGardGate.SessionCoordinator, [], restart: :temporary}
    ]
    Supervisor.start_link(children, strategy: :simple_one_for_one)
  end

  @doc """
  Registers a new worker, and creates the worker process
  """
  def register(args) do
    Supervisor.start_child(__MODULE__, [args])
  end
end
