defmodule ElvenGardTower.Supervisor do
  use Supervisor

  def start_link(_args) do
    children = [
      {ElvenGardTower.Datastore, []},
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
