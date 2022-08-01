defmodule Dmp.StringUtils do
  @moduledoc """
  Java.String- and Javascript-compatible functions missing in Elixir's `String` module.
  """

  use Bitwise, only_operators: true

  alias __MODULE__

  @doc """
  Returns the index within this string of the first occurrence of the specified substring,
  or -1 if there is no such occurence.

  ## Examples

      ies> StringUtils.index_of("", "")
      0

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

  ## Examples

      iex> StringUtils.index_of("abracadabra", "", 2)
      2

      iex> StringUtils.index_of("abracadabra", "", 100)
      11

      iex> StringUtils.index_of("abracadabra", "b", 2)
      8

      iex> StringUtils.index_of("abracadabra", "f", 2)
      -1

  """
  @spec index_of(String.t(), String.t(), non_neg_integer()) :: integer()
  def index_of(s, "", from_index), do: min(from_index, String.length(s))
  def index_of("", _, _), do: -1

  def index_of(s, str, from_index) when is_integer(from_index) and from_index >= 0 do
    case String.split(substring(s, from_index), str, parts: 2) do
      [before, _] -> String.length(before) + from_index
      _ -> -1
    end
  end

  @doc """
  Returns the index within this string of the last occurrence of the specified substring,
  or -1 if there is no such occurence.

  ## Examples

      ies> StringUtils.last_index_of("", "")
      0

      ies> StringUtils.last_index_of("abracadabra", "")
      11

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

  ## Examples

      iex> StringUtils.index_of("abracadabra", "b", 5)
      8

      iex> StringUtils.index_of("abracadabra", "d", 9)
      -1

      iex> StringUtils.last_index_of("abcdefghijk", "fgh", 5)
      5

      ies> StringUtils.last_index_of("abcdefghijk", "", 5)
      11

  """
  @spec last_index_of(String.t(), String.t(), non_neg_integer()) :: integer()
  def last_index_of(s, "", _), do: String.length(s)
  def last_index_of("", _, _), do: -1
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
  Returns a new string that is a substring of this string.

  The substring begins with the character at the specified index
  and extends to the end of this string.

  ## Examples

      iex> StringUtils.substring("abracadabra", 6)
      "dabra"

      iex> StringUtils.substring("abracadabra", 12)
      ""

  """
  @spec substring(String.t(), non_neg_integer()) :: String.t()
  def substring(s, begin_index), do: String.slice(s, begin_index..-1)

  @doc """
  Returns a new string that is a substring of this string.

  The substring begins at the specified `begin_index` and extends to the
  character at index `end_index - 1`. Thus the length of the substring
  is `end_index - begin_index`.

  ## Examples

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

  @not_escaped [
    {"+", " "},
    {"%2A", "*"}
  ]

  @doc """
  A URI encoding, but with spaces and asterisks left as is, for use with diffs.

  ## Examples

      iex> StringUtils.uri_encode("(")
      "%28"

      iex> StringUtils.uri_encode(" ")
      " "

      iex> StringUtils.uri_encode("*")
      "*"

      iex> StringUtils.uri_encode("[]")
      "%5B%5D"

  """
  @spec uri_encode(String.t()) :: String.t()
  def uri_encode(str) do
    str = URI.encode_www_form(str)
    Enum.reduce(@not_escaped, str, fn {from, to}, acc -> String.replace(acc, from, to) end)
  end

  @unescaped [
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
  `StringUtils.uri_encode` which returns uppercase hex.

  ## Examples

      iex> StringUtils.unescape_for_encode_uri_compatability("%3F")
      "?"

      iex> StringUtils.unescape_for_encode_uri_compatability("%3f")
      "%3f"

      iex> StringUtils.unescape_for_encode_uri_compatability("%24")
      "$"

  """
  @spec unescape_for_encode_uri_compatability(String.t()) :: String.t()
  def unescape_for_encode_uri_compatability(str) do
    Enum.reduce(@unescaped, str, fn {from, to}, acc -> String.replace(acc, from, to) end)
  end
end
