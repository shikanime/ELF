defmodule ElvenGardStdlib.PasswordPacket do
  @moduledoc """
  First PasswordRequest packet send by the client
  """

  defstruct user_password: nil

  @type t :: %__MODULE__{
          user_password: String.t(),
        }

  @spec parse!(binary) :: t
  def parse!(payload) do
    payload
    |> String.split()
    |> format()
  end

  defp format(payload) when length(payload) == 2 do
    %__MODULE__{
      user_password: Enum.at(payload, 1)
    }
  end
end
