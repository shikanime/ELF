defmodule ElvenGardBastion.Network do
  def parse_peername(socket) do
    {:ok, {addr, port}} = :inet.peername(socket)

    address =
      addr
      |> :inet_parse.ntoa()
      |> to_string()

    {address, port}
  end

  def reply({socket, transport, crypto}, packet) do
    transport.send(socket, packet)
    :ok
  end
end
