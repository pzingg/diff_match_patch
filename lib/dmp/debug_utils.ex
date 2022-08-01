defmodule Dmp.DebugUtils do
  @moduledoc """
  Utilities for debugging bitarrays.
  """

  use Bitwise, only_operators: true

  @doc """
  Prints the `alphabet` bitarray on IO, showing binary values.
  """
  @spec debug_alphabet(String.t(), Dmp.Match.alpha()) :: nil
  def debug_alphabet(pattern, s) do
    alphabet_header(pattern) |> IO.puts()
    pattern_length = String.length(pattern)

    String.codepoints(pattern)
    |> Enum.sort()
    |> Enum.dedup()
    |> Enum.map(fn ch -> alphabet_line(ch, s, pattern_length) |> IO.puts() end)

    nil
  end

  defp alphabet_header(pattern) do
    line = ["\n          alphabet:" | String.codepoints(pattern)]
    Enum.join(line, " ")
  end

  defp alphabet_line(ch, s, pattern_length) do
    ord = String.to_charlist(ch) |> List.first()
    value = Map.get(s, ord, 0)
    valstr = to_string(value) |> String.pad_leading(12)
    bits = bitmap_to_list(value, pattern_length)
    line = ["   ", ch, valstr <> ":"] ++ bits
    Enum.join(line, " ")
  end

  @doc """
  Prints the `rd` bitarray on IO, showing binary values.

  * `d` - Error level for the bitarray.
  * `start` - Lowest index that has been calculated.
  * `best_loc` - Index in the text where the best match has been found.
  """
  @spec debug_rd(
          String.t(),
          String.t(),
          non_neg_integer(),
          Dmp.Match.bitap_array(),
          non_neg_integer(),
          integer()
        ) :: nil
  def debug_rd(text, pattern, d, rd, start \\ 0, best_loc \\ -1) do
    rd_size = max(String.length(text) + 2, Map.fetch!(rd, -1))
    rd_header(d, pattern) |> IO.puts()
    pattern_length = String.length(pattern)

    Enum.map(0..(rd_size - 1), fn j ->
      ch =
        if j == 0 do
          nil
        else
          String.at(text, j - 1)
        end

      rd_j_line(ch, j, rd, pattern_length, start, best_loc) |> IO.puts()
    end)

    nil
  end

  defp rd_header(d, pattern) do
    dstr = "rd_j^#{d}" |> String.pad_trailing(7)
    line = ["#{dstr}    pattern:" | String.codepoints(pattern)]
    Enum.join(line, " ")
  end

  defp rd_j_line(nil, j, rd, pattern_length, start, best_loc) do
    rd_j_line("_", j, rd, pattern_length, start, best_loc)
  end

  defp rd_j_line(ch, j, rd, pattern_length, start, best_loc) do
    value = Map.get(rd, j, 0)
    valstr = to_string(value) |> String.pad_leading(12)
    jstr = to_string(j) |> String.pad_leading(2)

    jstr =
      cond do
        best_loc != -1 && j - 1 == best_loc ->
          jstr <> "@"

        j < start ->
          jstr <> "*"

        true ->
          jstr <> " "
      end

    bits = bitmap_to_list(value, pattern_length)
    line = [jstr, ch, valstr <> ":"] ++ bits
    Enum.join(line, " ")
  end

  @doc """
  Returns a list of "codepoints" (single-character strings)
  showing the base-2 value of `value`.

  A minimum of `padding` elements are returned.
  """
  @spec bitmap_to_list(non_neg_integer(), non_neg_integer()) :: [String.t()]
  def bitmap_to_list(value, padding \\ 0) do
    encode_bit_loop(value, "")
    |> String.pad_leading(padding, "0")
    |> String.codepoints()
  end

  defp encode_bit_loop(v, acc) do
    acc =
      if (v &&& 1) == 0 do
        "0" <> acc
      else
        "1" <> acc
      end

    v = v >>> 1

    if v == 0 do
      acc
    else
      encode_bit_loop(v, acc)
    end
  end
end
