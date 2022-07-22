defmodule DiffMatchPatch do
  @moduledoc false

  defmodule Options do
    @moduledoc """
    Defaults.
    Set these on your `DiffMatchPatch` instance to override the defaults.

    `:diff_timeout` - Number of seconds to map a diff before giving up (0 for infinity).
    `:diff_edit_cost` - Cost of an empty edit operation in terms of edit characters.
    `:match_threshold` - At what point is no match declared (0.0 = perfection, 1.0 = very loose).
    `:match_distance` - How far to search for a match (0 = exact location, 1000+ = broad match).
      A match this many characters away from the expected location will add
      1.0 to the score (0.0 is a perfect match).
    `:patch_delete_threshold` - When deleting a large block of text (over ~64 characters), how close do
      the contents have to be to match the expected contents. (0.0 = perfection,
      1.0 = very loose).  Note that Match_Threshold controls how closely the
      end points of a delete need to match.
    `:patch_margin` - Chunk size for context length.
    `:match_max_bits` - The number of bits in an int.
    """
    defstruct diff_timeout: 1.0,
              diff_edit_cost: 4,
              match_threshold: 0.5,
              match_distance: 1000,
              patch_delete_threshold: 0.5,
              patch_margin: 4,
              match_max_bits: 32

    @type t() :: %__MODULE__{
            diff_timeout: float(),
            diff_edit_cost: non_neg_integer(),
            match_threshold: float(),
            match_distance: non_neg_integer(),
            patch_delete_threshold: float(),
            patch_margin: non_neg_integer(),
            match_max_bits: non_neg_integer()
          }
  end

  # Mimic Java indexOf
  @doc """
  Returns the index within this string of the first occurrence of the specified substring,
  or -1 if there is no such occurence.
  """
  @spec index_of(String.t(), String.t()) :: integer()
  def index_of(s, str), do: index_of(s, str, 0)

  # Mimic Java indexOf
  @doc """
  Returns the index within this string of the first occurrence of the specified substring,
  starting the search at the specified index, or -1 if there is no such occurence.
  """
  @spec index_of(String.t(), String.t(), non_neg_integer()) :: integer()
  def index_of("", _, _), do: -1
  def index_of(_, "", _), do: -1

  def index_of(s, str, from_index) when is_integer(from_index) and from_index >= 0 do
    case String.split(substring(s, from_index), str, parts: 2) do
      [before, _] -> String.length(before) + from_index
      _ -> -1
    end
  end

  # Mimic Java lastIndexOf
  @doc """
  Returns the index within this string of the last occurrence of the specified substring,
  or -1 if there is no such occurence.
  """
  @spec last_index_of(String.t(), String.t()) :: integer()
  def last_index_of(s, str), do: last_index_of(s, str, 0)

  # Mimic Java lastIndexOf
  @doc """
  Returns the index within this string of the last occurrence of the specified substring,
  starting the search at the specified index, or -1 if there is no such occurence.
  """
  @spec last_index_of(String.t(), String.t(), non_neg_integer()) :: integer()
  def last_index_of("", _, _), do: -1
  def last_index_of(_, "", _), do: -1
  def last_index_of(s, str, begin_index) when begin_index < 0, do: last_index_of(s, str, 0)

  def last_index_of(s, str, begin_index) do
    s =
      if begin_index > 0 do
        String.slice(s, begin_index..-1)
      else
        s
      end

    s_length = String.length(s)
    str_length = String.length(str)

    cond do
      s_length < str_length ->
        -1

      s == str ->
        begin_index

      true ->
        Enum.reduce_while((s_length - str_length)..0//-1, -1, fn i, acc ->
          test = String.slice(s, i..(i + str_length - 1))

          if test == str do
            {:halt, i + begin_index}
          else
            {:cont, acc}
          end
        end)
    end
  end

  # Mimic Java substring
  @doc """
  Returns a new string that is a substring of this string. The substring begins
  with the character at the specified index and extends to the end of this string.
  """
  @spec substring(String.t(), non_neg_integer()) :: String.t()
  def substring(s, begin_index), do: String.slice(s, begin_index..-1)

  # Mimic Java substring
  @doc """
  Returns a new string that is a substring of this string. The substring begins
  at the specified `begin_index` and extends to the character at index `end_index - 1`.
  Thus the length of the substring is `end_index - begin_index`.
  """
  @spec substring(String.t(), non_neg_integer(), non_neg_integer()) :: String.t()
  def substring(s, begin_index, end_index) do
    if end_index > begin_index do
      String.slice(s, begin_index..(end_index - 1))
    else
      ""
    end
  end
end
