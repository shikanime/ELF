defmodule ElvenGardBastion.SignInPacket do
  @moduledoc """
  First SignInRequest packet send by the client
  """

  defstruct user_name: nil,
            user_password_hash: nil,
            client_version: nil,
            client_hash: nil

  @type t :: %__MODULE__{
          user_name: String.t(),
          user_password_hash: String.t(),
          client_version: String.t(),
          client_hash: String.t()
        }

  @spec parse(binary) :: t
  def parse(<<"NoS0575", payload::binary>>) do
    payload
    |> String.split()
    |> format()
  end

  defp format(payload) when length(payload) == 8 do
    %__MODULE__{
      user_name: Enum.at(payload, 1),
      user_password_hash: Enum.at(payload, 2),
      client_version: Enum.at(payload, 5),
      client_hash: Enum.at(payload, 7)
    }
  end
end
