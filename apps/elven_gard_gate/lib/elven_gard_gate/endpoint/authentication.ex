defmodule ElvenGardGate.Endpoint.Authentication do
  @default [transporter: [port: 4123], acceptor: 10]

  def child_spec(opts) do
    env = Application.get_env(:elven_gard_gate, LoginEndpoint, [])
    config = @default |> Keyword.merge(env) |> Keyword.merge(opts)

    :ranch.child_spec(
      ElvenGardGate.LoginEndpoint,
      config[:acceptor],
      :ranch_tcp,
      config[:transporter],
      ElvenGardGate.NostaleLoginProtocol,
      []
    )
  end
end
