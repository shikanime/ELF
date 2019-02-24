defmodule ElvenGard.WorldPacket do
  @moduledoc """
  First UsernameRequest packet send by the client
  """

  defstruct id: 0,
            data: %{}

  @type t :: %__MODULE__{
          id: integer(),
          data: struct()
        }
end
