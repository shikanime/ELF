defmodule ElvenGard.SessionRequest do
  @moduledoc """
  First LoginRequest packet send by the client
  """

  defstruct session_id: ""

  @type t :: %__MODULE__{
          session_id: String.t()
        }

  @spec parse!(binary) :: t
  def parse!(payload) do
    %__MODULE__{
      session_id: parse_session_id(payload)
    }
  end

  defp parse_session_id(session_id) do
    {numeric_session_id, ""} = Integer.parse(session_id)
    numeric_session_id
  end
end
