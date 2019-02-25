defmodule ElvenGardBastion.Endpoint.World do
  @default [transporter: [port: 4124], acceptor: 10]

  def child_spec(opts) do
    env = Application.get_env(:elven_gard_bastion, WorldEndpoint, [])
    config = @default |> Keyword.merge(env) |> Keyword.merge(opts)

    :ranch.child_spec(
      ElvenGardBastion.WorldEndpoint,
      config[:acceptor],
      :ranch_tcp,
      config[:transporter],
      ElvenGardBastion.NostaleWorldProtocol,
      []
    )
  end
end
