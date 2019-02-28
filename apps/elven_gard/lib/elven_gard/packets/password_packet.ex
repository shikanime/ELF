defmodule ElvenGard.PasswordPacket do
  @moduledoc """
  First PasswordRequest packet send by the client
  """

  defstruct user_password: nil

  @type t :: %__MODULE__{
          user_password: String.t(),
        }

  @spec parse(binary) :: t
  def parse(payload) do
    %__MODULE__{
      user_password: ElvenGard.PasswordCrypto.encrypt(payload)
    }
  end
end
