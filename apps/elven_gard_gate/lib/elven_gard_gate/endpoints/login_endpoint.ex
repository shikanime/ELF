defmodule ElvenGardGate.LoginEndpoint do
  def child_spec(opts) do
    env = [
      transporter: [
        port: Application.get_env(:elven_gard_gate, :port, 4123)
      ],
      acceptor: Application.get_env(:elven_gard_gate, :acceptor, 10)
    ]

    :ranch.child_spec(
      ElvenGardGate.LoginEndpoint,
      env[:acceptor],
      :ranch_tcp,
      env[:transporter] ++ Keyword.get(opts, :transporter, []),
      ElvenGardGate.NostaleLoginProtocol,
      Keyword.get(opts, :protocol, [])
    )
  end
end