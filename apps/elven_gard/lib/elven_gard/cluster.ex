defmodule ElvenGard.Cluster do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    children = [
      {Cluster.Supervisor, [cluster_config(), [name: ElvenGard.ClusterSupervisor]]},
    ]

    Supervisor.init(children, strategy: :one_for_one)
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
