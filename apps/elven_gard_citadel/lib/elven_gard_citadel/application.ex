defmodule ElvenGardCitadel.Application do
  use Application

  def start(_type, _args) do
    children = [
      ElvenGardCitadel.Datastore
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: ElvenGardCitadel.Supervisor)
  end
end
