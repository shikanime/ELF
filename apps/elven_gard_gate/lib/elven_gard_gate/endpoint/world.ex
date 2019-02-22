defmodule ElvenGardGate.Endpoint.World do
  @default [transporter: [port: 4124], acceptor: 10]

  def child_spec(opts) do
    env = Application.get_env(:elven_gard_gate, WorldEndpoint, [])
    config = @default |> Keyword.merge(env) |> Keyword.merge(opts)

    :ranch.child_spec(
      ElvenGardGate.WorldEndpoint,
      config[:acceptor],
      :ranch_tcp,
      config[:transporter],
      ElvenGardGate.NostaleWorldProtocol,
      []
    )
  end
end
