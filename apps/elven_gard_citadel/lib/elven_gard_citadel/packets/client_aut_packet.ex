defmodule ElvenGardCitadel.ClientAuthPacket do
  @moduledoc """
  First SignInRequest packet.reply by the client
  """

  defstruct client_id: "",
            session_id: ""

  @type t :: %__MODULE__{
          client_id: integer(),
          session_id: String.t()
        }

  @spec parse(binary) :: t
  def parse(payload) do
    %__MODULE__{
      client_id: parse_client_id(payload),
      session_id: UUID.uuid5(:nil, payload)
    }
  end

  defp parse_client_id(client_id) do
    {numeric_client_id, ""} = Integer.parse(client_id)
    numeric_client_id
  end
end
