defmodule ElvenGard.Endpoint.Gate do
  @default [transporter: [port: 4123], acceptor: 10]

  def child_spec(opts) do
    env = Application.get_env(:elven_gard, LoginEndpoint, [])
    config = @default |> Keyword.merge(env) |> Keyword.merge(opts)

    :ranch.child_spec(
      ElvenGard.LoginEndpoint,
      config[:acceptor],
      :ranch_tcp,
      config[:transporter],
      ElvenGard.NostaleLoginProtocol,
      []
    )
  end
end
