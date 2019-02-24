defmodule ElvenGard.UsernameRequest do
  @moduledoc """
  First UsernameRequest packet send by the client
  """

  alias ElvenGard.WorldPacket

  defstruct user_name: ""

  @type t :: %__MODULE__{
          user_name: String.t(),
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
        user_name: Enum.at(payload, 1)
      }
    }
  end
end
