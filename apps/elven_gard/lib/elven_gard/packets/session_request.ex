defmodule ElvenGard.SessionRequest do
  @moduledoc """
  First LoginRequest packet send by the client
  """

  defstruct client_id: ""

  @type t :: %__MODULE__{
          client_id: String.t()
        }

  @spec parse!(binary) :: t
  def parse!(<<_::size(8), payload::binary>>) do
    payload
    |> String.split()
    |> format()
  end

  defp format(payload) when length(payload) == 1 do
    %__MODULE__{
      client_id: Enum.at(payload, 0)
    }
  end
end
