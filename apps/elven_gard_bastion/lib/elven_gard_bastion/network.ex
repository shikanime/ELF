defmodule ElvenGardBastion.Network do
  def parse_peername(socket) do
    {:ok, {addr, port}} = :inet.peername(socket)

    address =
      addr
      |> :inet_parse.ntoa()
      |> to_string()

    {address, port}
  end
end
