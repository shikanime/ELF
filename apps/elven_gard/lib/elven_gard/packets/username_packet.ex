defmodule ElvenGard.UsernamePacket do
  @moduledoc """
  First UsernameRequest packet send by the client
  """

  defstruct user_name: nil

  @type t :: %__MODULE__{
          user_name: String.t(),
        }

  @spec parse(binary) :: t
  def parse(payload) do
    %__MODULE__{
      user_name: payload
    }
  end
end
