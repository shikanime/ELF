defmodule ElvenGardGate.Endpoint do
  use Supervisor

  alias ElvenGardGate.AuthenticationEndpoint
  alias ElvenGardGate.WorldEndpoint

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    children = [
      {AuthenticationEndpoint, []},
      {WorldEndpoint, []},
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
