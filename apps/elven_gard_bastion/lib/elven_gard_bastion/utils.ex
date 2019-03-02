defmodule ElvenGardBastion.Utils do
  def parse_peername(socket) do
    {:ok, {addr, port}} = :inet.peername(socket)

    address =
      addr
      |> :inet_parse.ntoa()
      |> to_string()

    {address, port}
  end

  def send(connection, view, name, packet) do
    packet = view.render(name, packet)

    if is_list(packet) do
      Enum.each(packet, &(do_send(connection, &1)))
    else
      do_send(connection, packet)
    end

    :ok
  end

  defp do_send({socket, transport, crypto}, packet) do
    packet = crypto.encrypt(packet)
    transport.send(socket, packet)
  end
end
