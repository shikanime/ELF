defmodule ElvenGard.PasswordCrypto do
  use Bitwise, only_operators: true

  @spec decrypt(binary) :: String.t()
  def decrypt(binary) do
    case binary |> String.length() |> rem(2) do
        0 -> String.slice(binary, 3..-1)
        1 -> String.slice(binary, 4..-1)
    end
    |> String.codepoints()
    |> Stream.chunk_every(2)
    |> Stream.map(fn [x | _] -> x end)
    |> Stream.chunk_every(2)
    |> Stream.map(&Enum.join/1)
    |> Enum.map(&String.to_integer(&1, 16))
    |> to_string
  end

  @spec encrypt(String.t()) :: binary
  def encrypt(binary) do
    :crypto.hash(:sha512, binary) |> Base.encode16()
  end
end
