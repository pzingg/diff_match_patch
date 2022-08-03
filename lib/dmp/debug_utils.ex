defmodule Dmp.DebugUtils do
  @moduledoc """
  Utilities for debugging bitarrays.
  """

  use Bitwise, only_operators: true

  @doc """
  Formats an alphabet bitarray into a list of lines, showing binary values.

  * `s` - The alphabet as returned by `Dmp.Match.alphabet/1`.
  * `pattern` - The pattern that is associated with the alphabet.

  ## Examples

      iex> DebugUtils.debug_alphabet(%{?a => 5, ?b => 2}, "aba")
      [
        "alphabet: a b a",
        "    a  5: 1 0 1",
        "    b  2: 0 1 0"
      ]

  """
  @spec debug_alphabet(Dmp.Match.alpha(), String.t()) :: [String.t()]
  def debug_alphabet(s, pattern) do
    pattern_length = String.length(pattern)
    int_len = value_size(pattern_length)

    data =
      String.codepoints(pattern)
      |> Enum.sort()
      |> Enum.dedup()
      |> Enum.map(fn ch -> alphabet_line(ch, s, pattern_length, int_len) end)

    [alphabet_header(pattern, int_len) | data]
  end

  defp alphabet_header(pattern, int_len) do
    label = String.pad_leading("alphabet:", int_len + 7)
    line = [label | String.codepoints(pattern)]
    Enum.join(line, " ")
  end

  defp alphabet_line(ch, s, pattern_length, int_len) do
    ord = String.to_charlist(ch) |> List.first()
    value = Map.get(s, ord, 0)
    valstr = format_int(value, int_len)
    bits = bitmap_to_list(value, pattern_length)
    line = ["   ", ch, valstr <> ":"] ++ bits
    Enum.join(line, " ")
  end

  @doc """
  Formats the `rd` bitarray into a list of lines, showing binary values.

  * `rd` - The bitarrary.
  * `text` - The text associated with `rd`.
  * `pattern` - The pattern associated with `rd`.
  * `d` - The error level.
  * `start` - The lowest index that has been calculated. Lines below this
    index will be marked with an asterisk.
  * `best_loc` - The index in the text where the best match has been found.
    The line at this index will be marked with an "@"-sign.

  ## Examples

      iex> DebugUtils.debug_rd(%{1 => 5, 2 => 7, -1 => 3}, "abc", "add", 0, 1, 2)
      [
        "  rd_j^0: a d d",
        " 0* _  0: 0 0 0",
        " 1  a  5: 1 0 1",
        " 2  b  7: 1 1 1",
        " 3@ c  0: 0 0 0",
        " 4  _  0: 0 0 0"
      ]

  """
  @spec debug_rd(
          Dmp.Match.bitap_array(),
          String.t(),
          String.t(),
          non_neg_integer(),
          non_neg_integer(),
          integer()
        ) :: [String.t()]
  def debug_rd(rd, text, pattern, d, start \\ 0, best_loc \\ -1) do
    pattern_length = String.length(pattern)
    int_len = value_size(pattern_length)
    rd_size = max(String.length(text) + 2, Map.fetch!(rd, -1))

    data =
      Enum.map(0..(rd_size - 1), fn j ->
        ch =
          if j == 0 do
            nil
          else
            String.at(text, j - 1)
          end

        rd_j_line(ch, j, rd, pattern_length, int_len, start, best_loc)
      end)

    [rd_header(d, pattern, int_len) | data]
  end

  defp rd_header(d, pattern, int_len) do
    label = "rd_j^#{d}:" |> String.pad_leading(int_len + 7)
    line = [label | String.codepoints(pattern)]
    Enum.join(line, " ")
  end

  defp rd_j_line(nil, j, rd, pattern_length, int_len, start, best_loc) do
    rd_j_line("_", j, rd, pattern_length, int_len, start, best_loc)
  end

  defp rd_j_line(ch, j, rd, pattern_length, int_len, start, best_loc) do
    value = Map.get(rd, j, 0)
    valstr = format_int(value, int_len)
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

  defp value_size(pattern_length) do
    largest_bitmap = (1 <<< pattern_length) - 1
    len = to_string(largest_bitmap) |> String.length()
    min(14, max(len, 2))
  end

  defp format_int(value, max_len) do
    valstr = to_string(value) |> String.pad_leading(max_len)

    if String.length(valstr) > max_len do
      String.pad_leading("<>", max_len)
    else
      valstr
    end
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
