defmodule ElvenGard.PasswordRequest do
  @moduledoc """
  First PasswordRequest packet send by the client
  """

  alias ElvenGard.WorldPacket

  defstruct password: ""

  @type t :: %__MODULE__{
          password: String.t(),
        }

  @spec parse!(binary) :: t
  def parse!(payload) do
    payload
    |> String.split()
    |> format()
  end

  defp format(payload) when length(payload) == 2 do
    %WorldPacket{
      id: Enum.at(payload, 0),
      data: %__MODULE__{
        password: Enum.at(payload, 1)
      }
    }
  end
end
