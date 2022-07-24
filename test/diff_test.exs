defmodule DiffTest do
  use ExUnit.Case

  alias Dmp.Diff

  # doctest Dmp.Diff

  defp one_second() do
    :os.system_time(:millisecond) + 1_000
  end

  # Construct the two texts which made up the diff originally.
  defp rebuild_texts(diffs) do
    Enum.reduce(diffs, {"", ""}, fn {op, text}, {text1, text2} ->
      case op do
        :insert ->
          {text1, text2 <> text}

        :delete ->
          {text1 <> text, text2}

        :equal ->
          {text1 <> text, text2 <> text}
      end
    end)
  end

  describe "common_prefix" do
    test "null case" do
      assert {"", "abc", "xyz"} == Diff.common_prefix("abc", "xyz")
    end

    test "non-null case" do
      assert {"1234", "abcdef", "xyz"} == Diff.common_prefix("1234abcdef", "1234xyz")
    end

    test "whole case" do
      assert {"1234", "", "xyz"} == Diff.common_prefix("1234", "1234xyz")
    end
  end

  describe "common_suffix" do
    test "null case" do
      assert {"", "abc", "xyz"} == Diff.common_suffix("abc", "xyz")
    end

    test "non-null case" do
      assert {"1234", "abcdef", "xyz"} == Diff.common_suffix("abcdef1234", "xyz1234")
    end

    test "whole case" do
      assert {"1234", "", "xyz"} == Diff.common_suffix("1234", "xyz1234")
    end
  end

  describe "common_overlap" do
    test "null case" do
      assert 0 == Diff.common_overlap("", "abcd")
    end

    test "whole case" do
      assert 3 == Diff.common_overlap("abc", "abcd")
    end

    test "no overlap" do
      assert 0 == Diff.common_overlap("123456", "abcd")
    end

    test "overlap" do
      assert 3 == Diff.common_overlap("123456xxx", "xxxabcd")
    end

    test "Unicode" do
      # Some overly clever languages (C#) may treat ligatures as equal to their
      # component letters.  E.g. U+FB01 == 'fi'
      assert 0 == Diff.common_overlap("fi", "\ufb01i")
    end
  end

  describe "half_match" do
    test "no match 1" do
      assert nil == Diff.half_match("1234567890", "abcdef", one_second())
    end

    test "no match 2" do
      assert nil == Diff.half_match("12345", "23", one_second())
    end

    test "single match 1" do
      assert {"12", "90", "a", "z", "345678"} ==
               Diff.half_match("1234567890", "a345678z", one_second())
    end

    test "single match 2" do
      assert {"a", "z", "12", "90", "345678"} ==
               Diff.half_match("a345678z", "1234567890", one_second())
    end

    test "single match 3" do
      assert {"abc", "z", "1234", "0", "56789"} ==
               Diff.half_match("abc56789z", "1234567890", one_second())
    end

    test "single match 4" do
      assert {"a", "xyz", "1", "7890", "23456"} ==
               Diff.half_match("a23456xyz", "1234567890", one_second())
    end

    test "mulitple match 1" do
      assert {"12123", "123121", "a", "z", "1234123451234"} ==
               Diff.half_match("121231234123451234123121", "a1234123451234z", one_second())
    end

    test "multiple match 2" do
      assert {"", "-=-=-=-=-=", "x", "", "x-=-=-=-=-=-=-="} ==
               Diff.half_match("x-=-=-=-=-=-=-=-=-=-=-=-=", "xx-=-=-=-=-=-=-=", one_second())
    end

    test "multiple match 3" do
      assert {"-=-=-=-=-=", "", "", "y", "-=-=-=-=-=-=-=y"} ==
               Diff.half_match("-=-=-=-=-=-=-=-=-=-=-=-=y", "-=-=-=-=-=-=-=yy", one_second())
    end
  end

  describe "lines_to_chars" do
    test "base case" do
      assert {"\x01\x02\x01", "\x02\x01\x02", ["", "alpha\n", "beta\n"]} ==
               Diff.lines_to_chars("alpha\nbeta\nalpha\n", "beta\nalpha\nbeta\n")
    end

    test "empty line" do
      assert {"", "\x01\x02\x03\x03", ["", "alpha\r\n", "beta\r\n", "\r\n"]} ==
               Diff.lines_to_chars("", "alpha\r\nbeta\r\n\r\n\r\n")
    end

    test "no newlines" do
      assert {"\x01", "\x02", ["", "a", "b"]} == Diff.lines_to_chars("a", "b")
    end

    test "8-bit code points" do
      # More than 256 to reveal any 8-bit limitations.
      n = 300
      chars = Enum.map(1..n, fn i -> to_string([i]) end) |> Enum.join("")
      line_list = Enum.map(1..n, fn i -> "#{i}\n" end)
      lines = Enum.join(line_list, "")
      line_list = ["" | line_list]
      assert {chars, "", line_list} == Diff.lines_to_chars(lines, "")
    end
  end

  describe "chars_to_lines" do
    test "base case" do
      diffs =
        [{:equal, "\x01\x02\x01"}, {:insert, "\x02\x01\x02"}]
        |> Diff.chars_to_lines(["", "alpha\n", "beta\n"])

      assert [{:equal, "alpha\nbeta\nalpha\n"}, {:insert, "beta\nalpha\nbeta\n"}] == diffs
    end

    test "8-bit code points" do
      # More than 256 to reveal any 8-bit limitations.
      n = 300
      chars = Enum.map(1..n, fn i -> to_string([i]) end) |> Enum.join("")
      line_list = Enum.map(1..n, fn i -> "#{i}\n" end)
      lines = Enum.join(line_list, "")
      line_list = ["" | line_list]
      diffs = [{:delete, chars}] |> Diff.chars_to_lines(line_list)
      assert [{:delete, lines}] == diffs
    end

    test "invalid code points" do
      # More than 55_295 to verify any 17 * 16-bit limitation.
      chars = Enum.map(55_290..55_299, fn i -> "#{i}\n" end) |> Enum.join("")
      {chars1, _chars2, line_list} = Diff.lines_to_chars(chars, "")

      [{_op, text}] = [{:insert, chars1}] |> Diff.chars_to_lines(line_list)
      assert chars == text
    end
  end

  describe "cleanup_merge" do
    @tag :good
    test "null case" do
      assert [] == Diff.cleanup_merge([])
    end

    @tag :good
    test "no change case" do
      diffs = [{:equal, "a"}, {:delete, "b"}, {:insert, "c"}]
      assert diffs == Diff.cleanup_merge(diffs)
    end

    @tag :good
    test "merge equalities" do
      diffs = [{:equal, "a"}, {:equal, "b"}, {:equal, "c"}]
      assert [{:equal, "abc"}] == Diff.cleanup_merge(diffs)
    end

    @tag :good
    test "merge deletions" do
      diffs = [{:delete, "a"}, {:delete, "b"}, {:delete, "c"}]
      assert [{:delete, "abc"}] == Diff.cleanup_merge(diffs)
    end

    @tag :good
    test "merge insertions" do
      diffs = [{:insert, "a"}, {:insert, "b"}, {:insert, "c"}]
      assert [{:insert, "abc"}] == Diff.cleanup_merge(diffs)
    end

    @tag :good
    test "merge interweave" do
      diffs = [
        {:delete, "a"},
        {:insert, "b"},
        {:delete, "c"},
        {:insert, "d"},
        {:equal, "e"},
        {:equal, "f"}
      ]

      assert [{:delete, "ac"}, {:insert, "bd"}, {:equal, "ef"}] == Diff.cleanup_merge(diffs)
    end

    test "prefix and suffix detection" do
      diffs = [{:delete, "a"}, {:insert, "abc"}, {:delete, "dc"}]

      assert [{:equal, "a"}, {:delete, "d"}, {:insert, "b"}, {:equal, "c"}] ==
               Diff.cleanup_merge(diffs)
    end
  end
end
