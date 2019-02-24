defmodule ElvenGard.LoginRequest do
  @moduledoc """
  First LoginRequest packet send by the client
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

  defp format(payload) when length(payload) == 8 do
    %__MODULE__{
      user_name: Enum.at(payload, 1),
      user_password: Enum.at(payload, 2),
      client_id: Enum.at(payload, 0),
      client_version: Enum.at(payload, 3),
      client_hash: Enum.at(payload, 5)
    }
  end
end
