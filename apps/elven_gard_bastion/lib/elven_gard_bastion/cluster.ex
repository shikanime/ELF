defmodule ElvenGardBastion.Cluster do
  use Supervisor

  def start_link(_args) do
    children = [
      {Cluster.Supervisor, [cluster_config(), [name: __MODULE__]]},
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def cluster_config do
    [
      gossip: [
        strategy: Elixir.Cluster.Strategy.Gossip,
        config: [
          secret: Application.get_env(:libcluster, :gossip_secret, System.get_env("GOSSIP_SECRET"))
        ]
      ]
    ]
  end
end
