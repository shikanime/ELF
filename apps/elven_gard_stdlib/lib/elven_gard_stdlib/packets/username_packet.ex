defmodule ElvenGardStdlib.UsernamePacket do
  @moduledoc """
  First UsernameRequest packet send by the client
  """

  defstruct user_name: nil

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
    %__MODULE__{
      user_name: Enum.at(payload, 1)
    }
  end
end
