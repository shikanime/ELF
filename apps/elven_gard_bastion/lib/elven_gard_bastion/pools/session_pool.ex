defmodule ElvenGardBastion.SessionPool do
  use DynamicSupervisor

  def start_link(args) do
    DynamicSupervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def register() do
    DynamicSupervisor.start_child(__MODULE__, {ElvenGardBastion.SessionSocket, []})
  end
end
