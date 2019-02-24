defmodule ElvenGardTower.Application do
  use Application

  def start(_type, _args) do
    children = [
      ElvenGardTower.Datastore
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: ElvenGardTower.Supervisor)
  end
end
