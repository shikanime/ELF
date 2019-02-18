defmodule ElvenGardAuth.WorldCrypto do
  use Bitwise, only_operators: true

  @world_chunk_table [" ", "-", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "n"]

  @doc false
  @spec encrypt!(binary) :: binary
  def encrypt!(packet) do
    bytes =
      packet
      |> to_charlist
      |> Enum.with_index()

    length = byte_size(packet)
    data = for {b, i} <- bytes, into: <<>>, do: do_encrypt(b, i, length)
    <<data::binary, 0xFF::size(8)>>
  end

  @doc false
  @spec decrypt!(binary, integer, boolean) :: [binary | {integer, binary}]
  def decrypt!(binary, session_id, keepalive? \\ false) do
    session_key = session_id &&& 0xFF
    offset = session_key + 0x40 &&& 0xFF
    switch = session_id >>> 6 &&& 0x03

    packets =
      for <<c <- binary>>, into: <<>> do
        char =
          case switch do
            0 -> c - offset
            1 -> c + offset
            2 -> (c - offset) ^^^ 0xC3
            3 -> (c + offset) ^^^ 0xC3
          end

        <<char::size(8)>>
      end

    result =
      :binary.split(packets, <<0xFF>>, [:global, :trim_all])
      |> Enum.map(&do_decrypt/1)

    case keepalive? do
      false ->
        result

      true ->
        result
        |> Stream.map(&String.split(&1, " ", parts: 2))
        |> Enum.map(fn [l, r] -> {String.to_integer(l), r} end)
    end
  end

  @doc false
  @spec do_encrypt(char, integer, integer) :: binary
  defp do_encrypt(char, index, _) when rem(index, 0x7E) != 0, do: <<(~~~char)>>

  defp do_encrypt(char, index, length) do
    remaining = if length - index > 0x7E, do: 0x7E, else: length - index
    <<remaining::size(8), ~~~char::size(8)>>
  end

  @doc false
  @spec do_decrypt(binary, list) :: String.t()
  defp do_decrypt(binary, result \\ [])
  defp do_decrypt("", result), do: result |> Enum.reverse() |> Enum.join()

  defp do_decrypt(<<byte::size(8), rest::binary>>, result) when byte <= 0x7A do
    len = Enum.min([byte, byte_size(rest)])
    {first, second} = String.split_at(rest, len)
    res = for <<c <- first>>, into: "", do: <<c ^^^ 0xFF>>
    do_decrypt(second, [res | result])
  end

  defp do_decrypt(<<byte::size(8), rest::binary>>, result) do
    len = byte &&& 0x7F
    {first, second} = do_decrypt_chunk(rest, len)
    do_decrypt(second, [first | result])
  end

  @spec do_decrypt_chunk(binary, integer, binary) :: {binary, binary}
  defp do_decrypt_chunk(bin, len, i \\ 0, result \\ "")
  defp do_decrypt_chunk("", _, _, result), do: {result, ""}
  defp do_decrypt_chunk(bin, len, i, result) when i >= len, do: {result, bin}

  defp do_decrypt_chunk(bin, len, i, result) do
    <<h::size(4), l::size(4), rest::binary>> = bin

    res =
      cond do
        h != 0 and h != 0xF and (l == 0 or l == 0xF) ->
          Enum.at(@world_chunk_table, h - 1)

        l != 0 and l != 0xF and (h == 0 or h == 0xF) ->
          Enum.at(@world_chunk_table, l - 1)

        h != 0 and h != 0xF and l != 0 and l != 0xF ->
          Enum.at(@world_chunk_table, h - 1) <> Enum.at(@world_chunk_table, l - 1)

        true ->
          ""
      end

    case h != 0 and h != 0xF do
      true -> do_decrypt_chunk(rest, len, i + 2, result <> res)
      false -> do_decrypt_chunk(rest, len, i + 1, result <> res)
    end
  end
end
