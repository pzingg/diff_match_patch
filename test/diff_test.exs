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
    test "null case" do
      assert [] == Diff.cleanup_merge([])
    end

    test "no change" do
      diffs = [{:equal, "a"}, {:delete, "b"}, {:insert, "c"}]
      assert diffs == Diff.cleanup_merge(diffs)
    end

    test "merge equalities" do
      diffs = [{:equal, "a"}, {:equal, "b"}, {:equal, "c"}]
      assert [{:equal, "abc"}] == Diff.cleanup_merge(diffs)
    end

    test "merge deletions" do
      diffs = [{:delete, "a"}, {:delete, "b"}, {:delete, "c"}]
      assert [{:delete, "abc"}] == Diff.cleanup_merge(diffs)
    end

    test "merge insertions" do
      diffs = [{:insert, "a"}, {:insert, "b"}, {:insert, "c"}]
      assert [{:insert, "abc"}] == Diff.cleanup_merge(diffs)
    end

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

    test "prefix and suffix detection with equalities" do
      diffs = [{:equal, "x"}, {:delete, "a"}, {:insert, "abc"}, {:delete, "dc"}, {:equal, "y"}]

      assert [{:equal, "xa"}, {:delete, "d"}, {:insert, "b"}, {:equal, "cy"}] ==
               Diff.cleanup_merge(diffs)
    end

    test "slide edit left" do
      diffs = [{:equal, "a"}, {:insert, "ba"}, {:equal, "c"}]
      assert [{:insert, "ab"}, {:equal, "ac"}] == Diff.cleanup_merge(diffs)
    end

    test "slide edit right" do
      diffs = [{:equal, "c"}, {:insert, "ab"}, {:equal, "a"}]
      assert [{:equal, "ca"}, {:insert, "ba"}] == Diff.cleanup_merge(diffs)
    end

    test "slide edit left recursive" do
      diffs = [{:equal, "a"}, {:delete, "b"}, {:equal, "c"}, {:delete, "ac"}, {:equal, "x"}]
      assert [{:delete, "abc"}, {:equal, "acx"}] == Diff.cleanup_merge(diffs)
    end

    test "slide edit right recursive" do
      diffs = [{:equal, "x"}, {:delete, "ca"}, {:equal, "c"}, {:delete, "b"}, {:equal, "a"}]
      assert [{:equal, "xca"}, {:delete, "cba"}] == Diff.cleanup_merge(diffs)
    end

    test "empty merge" do
      diffs = [{:delete, "b"}, {:insert, "ab"}, {:equal, "c"}]
      assert [{:insert, "a"}, {:equal, "bc"}] == Diff.cleanup_merge(diffs)
    end

    test "empty equality" do
      diffs = [{:equal, ""}, {:insert, "a"}, {:equal, "b"}]
      assert [{:insert, "a"}, {:equal, "b"}] == Diff.cleanup_merge(diffs)
    end
  end

  # Slide diffs to match logical boundaries.
  describe "cleanup_semantic_lossless" do
    test "null case" do
      assert [] == Diff.cleanup_semantic_lossless([])
    end

    test "blank lines" do
      diffs = [{:equal, "AAA\r\n\r\nBBB"}, {:insert, "\r\nDDD\r\n\r\nBBB"}, {:equal, "\r\nEEE"}]

      assert [{:equal, "AAA\r\n\r\n"}, {:insert, "BBB\r\nDDD\r\n\r\n"}, {:equal, "BBB\r\nEEE"}] ==
               Diff.cleanup_semantic_lossless(diffs)
    end

    test "line boundaries" do
      diffs = [{:equal, "AAA\r\nBBB"}, {:insert, " DDD\r\nBBB"}, {:equal, " EEE"}]

      assert [{:equal, "AAA\r\n"}, {:insert, "BBB DDD\r\n"}, {:equal, "BBB EEE"}] ==
               Diff.cleanup_semantic_lossless(diffs)
    end

    test "word boundaries" do
      diffs = [{:equal, "The c"}, {:insert, "ow and the c"}, {:equal, "at."}]

      assert [{:equal, "The "}, {:insert, "cow and the "}, {:equal, "cat."}] ==
               Diff.cleanup_semantic_lossless(diffs)
    end

    test "alphanumeric boundaries" do
      diffs = [{:equal, "The-c"}, {:insert, "ow-and-the-c"}, {:equal, "at."}]

      assert [{:equal, "The-"}, {:insert, "cow-and-the-"}, {:equal, "cat."}] ==
               Diff.cleanup_semantic_lossless(diffs)
    end

    test "hitting the start" do
      diffs = [{:equal, "a"}, {:delete, "a"}, {:equal, "ax"}]
      assert [{:delete, "a"}, {:equal, "aax"}] == Diff.cleanup_semantic_lossless(diffs)
    end

    test "hitting the end" do
      diffs = [{:equal, "xa"}, {:delete, "a"}, {:equal, "a"}]
      assert [{:equal, "xaa"}, {:delete, "a"}] == Diff.cleanup_semantic_lossless(diffs)
    end

    test "sentence boundaries" do
      diffs = [{:equal, "The xxx. The "}, {:insert, "zzz. The "}, {:equal, "yyy."}]

      assert [{:equal, "The xxx."}, {:insert, " The zzz."}, {:equal, " The yyy."}] ==
               Diff.cleanup_semantic_lossless(diffs)
    end
  end

  describe "cleanup semantically trivial equalities" do
    test "null case" do
      assert [] == Diff.cleanup_semantic([])
    end

    test "no elimination #1" do
      diffs = [{:delete, "ab"}, {:insert, "cd"}, {:equal, "12"}, {:delete, "e"}]

      assert [{:delete, "ab"}, {:insert, "cd"}, {:equal, "12"}, {:delete, "e"}] ==
               Diff.cleanup_semantic(diffs)
    end

    test "no elimination #2" do
      diffs = [{:delete, "abc"}, {:insert, "ABC"}, {:equal, "1234"}, {:delete, "wxyz"}]

      assert [{:delete, "abc"}, {:insert, "ABC"}, {:equal, "1234"}, {:delete, "wxyz"}] ==
               Diff.cleanup_semantic(diffs)
    end

    test "simple elimination" do
      diffs = [{:delete, "a"}, {:equal, "b"}, {:delete, "c"}]
      assert [{:delete, "abc"}, {:insert, "b"}] == Diff.cleanup_semantic(diffs)
    end

    test "backpass elimination" do
      diffs = [{:delete, "ab"}, {:equal, "cd"}, {:delete, "e"}, {:equal, "f"}, {:insert, "g"}]
      assert [{:delete, "abcdef"}, {:insert, "cdfg"}] == Diff.cleanup_semantic(diffs)
    end

    test "multiple eliminations" do
      diffs = [
        {:insert, "1"},
        {:equal, "A"},
        {:delete, "B"},
        {:insert, "2"},
        {:equal, "_"},
        {:insert, "1"},
        {:equal, "A"},
        {:delete, "B"},
        {:insert, "2"}
      ]

      assert [{:delete, "AB_AB"}, {:insert, "1A2_1A2"}] == Diff.cleanup_semantic(diffs)
    end

    test "word boundaries" do
      diffs = [{:equal, "The c"}, {:delete, "ow and the c"}, {:equal, "at."}]

      assert [{:equal, "The "}, {:delete, "cow and the "}, {:equal, "cat."}] ==
               Diff.cleanup_semantic(diffs)
    end

    test "no overlap elimination" do
      diffs = [{:delete, "abcxx"}, {:insert, "xxdef"}]
      assert [{:delete, "abcxx"}, {:insert, "xxdef"}] == Diff.cleanup_semantic(diffs)
    end

    test "overlap elimination" do
      diffs = [{:delete, "abcxxx"}, {:insert, "xxxdef"}]
      assert [{:delete, "abc"}, {:equal, "xxx"}, {:insert, "def"}] == Diff.cleanup_semantic(diffs)
    end

    test "reverse overlap elimination" do
      diffs = [{:delete, "xxxabc"}, {:insert, "defxxx"}]
      assert [{:insert, "def"}, {:equal, "xxx"}, {:delete, "abc"}] == Diff.cleanup_semantic(diffs)
    end

    test "two overlap eliminations" do
      diffs = [
        {:delete, "abcd1212"},
        {:insert, "1212efghi"},
        {:equal, "----"},
        {:delete, "A3"},
        {:insert, "3BC"}
      ]

      assert [
               {:delete, "abcd"},
               {:equal, "1212"},
               {:insert, "efghi"},
               {:equal, "----"},
               {:delete, "A"},
               {:equal, "3"},
               {:insert, "BC"}
             ] == Diff.cleanup_semantic(diffs)
    end
  end

  # Use default edit_cost 4
  describe "cleanup operationally trivial equalities" do
    test "null case" do
      assert [] == Diff.cleanup_efficiency([], 4)
    end

    test "no elimination" do
      diffs = [
        {:delete, "ab"},
        {:insert, "12"},
        {:equal, "wxyz"},
        {:delete, "cd"},
        {:insert, "34"}
      ]

      assert [
               {:delete, "ab"},
               {:insert, "12"},
               {:equal, "wxyz"},
               {:delete, "cd"},
               {:insert, "34"}
             ] == Diff.cleanup_efficiency(diffs, 4)
    end

    test "four-edit elimination" do
      diffs = [
        {:delete, "ab"},
        {:insert, "12"},
        {:equal, "xyz"},
        {:delete, "cd"},
        {:insert, "34"}
      ]

      assert [{:delete, "abxyzcd"}, {:insert, "12xyz34"}] == Diff.cleanup_efficiency(diffs, 4)
    end

    test "three-edit elimination" do
      diffs = [{:insert, "12"}, {:equal, "x"}, {:delete, "cd"}, {:insert, "34"}]
      assert [{:delete, "xcd"}, {:insert, "12x34"}] == Diff.cleanup_efficiency(diffs, 4)
    end

    test "backpass elimination" do
      diffs = [
        {:delete, "ab"},
        {:insert, "12"},
        {:equal, "xy"},
        {:insert, "34"},
        {:equal, "z"},
        {:delete, "cd"},
        {:insert, "56"}
      ]

      assert [{:delete, "abxyzcd"}, {:insert, "12xy34z56"}] == Diff.cleanup_efficiency(diffs, 4)
    end

    # use edit_cost 5
    test "high cost elimination" do
      diffs = [
        {:delete, "ab"},
        {:insert, "12"},
        {:equal, "wxyz"},
        {:delete, "cd"},
        {:insert, "34"}
      ]

      assert [{:delete, "abwxyzcd"}, {:insert, "12wxyz34"}] == Diff.cleanup_efficiency(diffs, 5)
    end
  end
end
