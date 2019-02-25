defmodule ElvenGardStdlib.PasswordCrypto do
  @moduledoc """
  Cryptography for a Nostale login server.
  """

  use Bitwise, only_operators: true

  @doc """
  Encrypt a login binary.

  ## Examples

      iex> ElvenGardStdlib.LoginCrypto.encrypt("fail Hello. This is a basic test")
      <<117, 112, 120, 123, 47, 87, 116, 123, 123, 126, 61, 47, 99, 119, 120, 130, 47, 120, 130, 47, 112, 47, 113, 112, 130, 120, 114, 47, 131, 116, 130, 131, 25>>

  """
  @spec encrypt(String.t()) :: binary
  def encrypt(binary) do
    :crypto.hash(:sha512, binary) |> Base.encode16(case: :lower) |> String.upcase
  end
end
