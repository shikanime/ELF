defmodule ElvenGardStdlib.ClientAuthPacket do
  @moduledoc """
  First LoginRequest packet send by the client
  """

  defstruct client_id: ""

  @type t :: %__MODULE__{
          client_id: String.t()
        }

  @spec parse!(binary) :: t
  def parse!(payload) do
    %__MODULE__{
      client_id: parse_client_id(payload)
    }
  end

  defp parse_client_id(client_id) do
    {numeric_client_id, ""} = Integer.parse(client_id)
    numeric_client_id
  end
end
