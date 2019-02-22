defmodule ElvenGard.LobbyCrypto do
  use Bitwise, only_operators: true

  @doc """
  Decrypt the first packet who contains the session_id
  """
  @spec decrypt!(binary) :: {integer, integer}
  def decrypt!(<<_::size(8), payload::binary>>) do
    do_decrypt(payload)
    |> String.split()
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  @doc false
  @spec do_decrypt(binary, String.t()) :: String.t()
  defp do_decrypt(binary, result \\ "")
  defp do_decrypt(<<>>, result), do: result
  defp do_decrypt(<<0xE::size(8), _::binary>>, result), do: result

  defp do_decrypt(<<char::size(8), rest::binary>>, result) do
    first_byte = char - 0xF
    second_byte = first_byte &&& 0xF0
    first_key = first_byte - second_byte
    second_key = second_byte >>> 0x4

    first =
      case second_key do
        0 -> " "
        1 -> " "
        2 -> "-"
        3 -> "."
        _ -> <<0x2C + second_key::utf8>>
      end

    second =
      case first_key do
        0 -> " "
        1 -> " "
        2 -> "-"
        3 -> "."
        _ -> <<0x2C + first_key::utf8>>
      end

    do_decrypt(rest, result <> first <> second)
  end
end
