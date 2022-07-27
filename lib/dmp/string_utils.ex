defmodule Dmp.StringUtils do
  @moduledoc """
  Java.String compatible functions.
  """

  alias __MODULE__

  @doc """
  Returns the index within this string of the first occurrence of the specified substring,
  or -1 if there is no such occurence.

  Examples:

      iex> StringUtils.index_of("abracadabra", "b")
      1

      iex> StringUtils.index_of("abracadabra", "f")
      -1

  """
  @spec index_of(String.t(), String.t()) :: integer()
  def index_of(s, str), do: index_of(s, str, 0)

  @doc """
  Returns the index within this string of the first occurrence of the specified substring,
  starting the search at the specified index, or -1 if there is no such occurence.

  Examples:

      iex> StringUtils.index_of("abracadabra", "b", 2)
      8

      iex> StringUtils.index_of("abracadabra", "f", 2)
      -1

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

  @doc """
  Returns the index within this string of the last occurrence of the specified substring,
  or -1 if there is no such occurence.

  Examples:

      iex> StringUtils.last_index_of("abracadabra", "b")
      8

      iex> StringUtils.last_index_of("abracadabra", "f")
      -1

  """
  @spec last_index_of(String.t(), String.t()) :: integer()
  def last_index_of(s, str), do: last_index_of(s, str, 0)

  @doc """
  Returns the index within this string of the last occurrence of the specified substring,
  starting the search at the specified index, or -1 if there is no such occurence.

  Examples:

      iex> StringUtils.index_of("abracadabra", "b", 5)
      8

      iex> StringUtils.index_of("abracadabra", "d", 9)
      -1

      iex> StringUtils.last_index_of("abcdefghijk", "fgh", 5)
      5

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

    s_rev = String.reverse(s)
    str_rev = String.reverse(str)

    case index_of(s_rev, str_rev) do
      -1 -> -1
      i -> begin_index + (String.length(s_rev) - String.length(str_rev) - i)
    end
  end

  @doc """
  Returns a new string that is a substring of this string. The substring begins
  with the character at the specified index and extends to the end of this string.

  Examples:

      iex> StringUtils.substring("abracadabra", 6)
      "dabra"

      iex> StringUtils.substring("abracadabra", 12)
      ""

  """
  @spec substring(String.t(), non_neg_integer()) :: String.t()
  def substring(s, begin_index), do: String.slice(s, begin_index..-1)

  # Mimic Java substring
  @doc """
  Returns a new string that is a substring of this string. The substring begins
  at the specified `begin_index` and extends to the character at index `end_index - 1`.
  Thus the length of the substring is `end_index - begin_index`.

  Examples:

      iex> StringUtils.substring("abracadabra", 2, 6)
      "raca"

      iex> StringUtils.substring("abracadabra", 2, 12)
      "racadabra"

  """
  @spec substring(String.t(), non_neg_integer(), non_neg_integer()) :: String.t()
  def substring(s, begin_index, end_index) do
    if end_index > begin_index do
      String.slice(s, begin_index..(end_index - 1))
    else
      ""
    end
  end

  @doc """
  A URI encoding, but with spaces left as is, for use with diffs.
  """
  @spec uri_encode(String.t()) :: String.t()
  def uri_encode(str) do
    URI.encode(str) |> String.replace("%20", " ")
  end

  @encoding_map [
    {"%21", "!"},
    {"%7E", "~"},
    {"%27", "'"},
    {"%28", "("},
    {"%29", ")"},
    {"%3B", ";"},
    {"%2F", "/"},
    {"%3F", "?"},
    {"%3A", ":"},
    {"%40", "@"},
    {"%26", "&"},
    {"%3D", "="},
    {"%2B", "+"},
    {"%24", "$"},
    {"%2C", ","},
    {"%23", "#"}
  ]

  @doc """
  Unescape selected chars for compatability with JavaScript's `encodeURI`.
  In speed critical applications this could be dropped since the
  receiving application will certainly decode these fine.
  Note that this function is case-sensitive.  Thus "%3f" would not be
  unescaped.  But this is ok because it is only called with the output of
  `uri_encode` which returns uppercase hex.

  Example: "%3F" -> "?", "%24" -> "$", etc.

  `str` The string to escape.

  Returns the unescaped string.
  """
  @spec unescape_for_encode_uri_compatability(String.t()) :: String.t()
  def unescape_for_encode_uri_compatability(str) do
    Enum.reduce(@encoding_map, str, fn {from, to}, acc -> String.replace(acc, from, to) end)
  end
end
