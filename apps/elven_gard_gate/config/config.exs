use Mix.Config

config :libcluster,
  topologies: [
    gossip_example: [
      strategy: Elixir.Cluster.Strategy.Gossip,
      config: [
        port: 45892,
        if_addr: "0.0.0.0",
        multicast_addr: "230.1.1.251",
        multicast_ttl: 1,
        secret: "3aS00tjjpwMK7BmzKpdR7PffN323i9439IEUPYdLTGKRH7VatPPTSaM5qKcu4/HM"
      ]
    ]
  ]
