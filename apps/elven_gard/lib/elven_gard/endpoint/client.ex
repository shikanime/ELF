defmodule ElvenGard.Endpoint.Client do
  defstruct transporter: nil,
            socket: nil

  def reply(client, socket, msgs) when is_list(msgs) do
    msgs
    |> Enum.map(&reply(client, socket, &1))
    |> Enum.find(:ok, fn
      :ok -> false
      _ -> true
    end)
  end

  def reply(client, socket, msg) do
    client.transporter.send(socket, msg)
  end

  def close(client, socket) do
    client.transporter.close(socket)
  end
end
