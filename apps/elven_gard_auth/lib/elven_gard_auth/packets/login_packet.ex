defmodule ElvenGardAuth.LoginPacket do
  @moduledoc """
  First LoginPacket packet send by the client
  """

  defstruct user_name: "",
            user_password: "",
            client_id: "",
            client_version: "",
            client_hash: ""

  @type t :: %__MODULE__{
          user_name: String.t(),
          user_password: String.t(),
          client_id: String.t(),
          client_version: String.t(),
          client_hash: String.t()
        }

  @spec parse!(binary) :: t
  def parse!(<<"NoS0575", payload::binary>>) do
    payload
    |> String.split()
    |> format()
  end

  defp format(payload) when length(payload) == 5 do
    %__MODULE__{
      user_name: payload[1],
      user_password: decode_password(payload[2]),
      client_id: payload[0],
      client_version: payload[3],
      client_hash: payload[5]
    }
  end

  defp decode_password(password) do
    case password |> String.length() |> rem(2) do
      0 -> String.slice(password, 3..-1)
      1 -> String.slice(password, 4..-1)
    end
    |> String.codepoints()
    |> Stream.chunk_every(2)
    |> Stream.map(fn [x | _] -> x end)
    |> Stream.chunk_every(2)
    |> Stream.map(&Enum.join/1)
    |> Enum.map(&String.to_integer(&1, 16))
    |> to_string
  end
end
