defmodule ElvenGard.Endpoint.World do
  @default [transporter: [port: 4124], acceptor: 10]

  def child_spec(opts) do
    env = Application.get_env(:elven_gard, WorldEndpoint, [])
    config = @default |> Keyword.merge(env) |> Keyword.merge(opts)

    :ranch.child_spec(
      ElvenGard.WorldEndpoint,
      config[:acceptor],
      :ranch_tcp,
      config[:transporter],
      ElvenGard.NostaleWorldProtocol,
      []
    )
  end
end
