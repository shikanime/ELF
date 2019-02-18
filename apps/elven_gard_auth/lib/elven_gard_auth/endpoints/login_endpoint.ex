defmodule ElvenGardAuth.LoginEndpoint do
  def child_spec(opts) do
    env = [
      transporter: [
        port: Application.get_env(:elven_gard_auth, :port, 4123)
      ],
      acceptor: Application.get_env(:elven_gard_auth, :acceptor, 10)
    ]

    :ranch.child_spec(
      ElvenGardAuth.LoginEndpoint,
      env[:acceptor],
      :ranch_tcp,
      env[:transporter] ++ Keyword.get(opts, :transporter, []),
      ElvenGardAuthNostaleLoginProtocol,
      Keyword.get(opts, :protocol, [])
    )
  end
end
