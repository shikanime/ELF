defmodule ElvenGardBastion.Endpoint.Gate do
  @default [transporter: [port: 4123], acceptor: 10]

  def child_spec(opts) do
    env = Application.get_env(:elven_gard_bastion, LoginEndpoint, [])
    config = @default |> Keyword.merge(env) |> Keyword.merge(opts)

    :ranch.child_spec(
      ElvenGardBastion.LoginEndpoint,
      config[:acceptor],
      :ranch_tcp,
      config[:transporter],
      ElvenGardBastion.NostaleLoginProtocol,
      []
    )
  end
end
