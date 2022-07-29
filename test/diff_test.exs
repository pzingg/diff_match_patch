defmodule DiffTest do
  use ExUnit.Case

  import Dmp.StringUtils

  alias Dmp.{Diff, Options}

  doctest Dmp.Diff

  defp one_second() do
    :os.system_time(:millisecond) + 1_000
  end

  defp with_timeout(secs) do
    opts = %Options{}
    %Options{opts | diff_timeout: secs}
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
      chars = Enum.map_join(1..n, fn i -> to_string([i]) end)
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
      chars = Enum.map_join(1..n, fn i -> to_string([i]) end)
      line_list = Enum.map(1..n, fn i -> "#{i}\n" end)
      lines = Enum.join(line_list, "")
      line_list = ["" | line_list]
      diffs = [{:delete, chars}] |> Diff.chars_to_lines(line_list)
      assert [{:delete, lines}] == diffs
    end

    test "invalid code points" do
      # More than 55_295 to verify any 17 * 16-bit limitation.
      chars = Enum.map_join(55_290..55_299, fn i -> "#{i}\n" end)
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

  test "pretty print" do
    diffs = [{:equal, "a\n"}, {:delete, "<B>b</B>"}, {:insert, "c&d"}]

    assert "<span>a&para;<br></span><del style=\"background:#ffe6e6;\">&lt;B&gt;b&lt;/B&gt;</del><ins style=\"background:#e6ffe6;\">c&amp;d</ins>" ==
             Diff.pretty_html(diffs)
  end

  test "compute the source and destination texts" do
    diffs = [
      {:equal, "jump"},
      {:delete, "s"},
      {:insert, "ed"},
      {:equal, " over "},
      {:delete, "the"},
      {:insert, "a"},
      {:equal, " lazy"}
    ]

    assert "jumps over the lazy" == Diff.text1(diffs)
    assert "jumped over a lazy" == Diff.text2(diffs)
  end

  describe "delta" do
    test "normal case" do
      diffs = [
        {:equal, "jump"},
        {:delete, "s"},
        {:insert, "ed"},
        {:equal, " over "},
        {:delete, "the"},
        {:insert, "a"},
        {:equal, " lazy"},
        {:insert, "old dog"}
      ]

      text1 = Diff.text1(diffs)
      assert "jumps over the lazy" == text1

      delta = Diff.to_delta(diffs)
      assert "=4\t-1\t+ed\t=6\t-3\t+a\t=5\t+old dog" == delta

      # Convert delta string into a diff.
      assert diffs == Diff.from_delta(text1, delta)

      # Generates error (19 != 20).
      assert_raise ArgumentError, "Delta length (19) smaller than source text length (20)", fn ->
        Diff.from_delta(text1 <> "x", delta)
      end

      # Generates error (19 != 18).
      text1 = substring(text1, 1)

      assert_raise ArgumentError, "Delta length (19) larger than source text length (18)", fn ->
        Diff.from_delta(text1, delta)
      end
    end

    # String "+%c3xy" passes in Elixir
    # test "invalid Unicode" do
    #  assert_raise ArgumentError, "", fn -> Diff.from_delta("", "+%c3xy") end
    # end

    test "special characters" do
      diffs = [
        {:equal, "\u0680 \x00 \t %"},
        {:delete, "\u0681 \x01 \n ^"},
        {:insert, "\u0682 \x02 \\ |"}
      ]

      text1 = Diff.text1(diffs)
      assert "\u0680 \x00 \t %\u0681 \x01 \n ^" == text1

      delta = Diff.to_delta(diffs)
      assert "=7\t-7\t+%DA%82 %02 %5C %7C" == delta

      # Convert delta string into a diff.
      assert diffs == Diff.from_delta(text1, delta)
    end

    test "verify pool of unchanged characters" do
      diffs = [{:insert, "A-Z a-z 0-9 - _ . ! ~ * ' ( ) ; / ? : @ & = + $ , # "}]
      text2 = Diff.text2(diffs)
      assert "A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # " == text2

      delta = Diff.to_delta(diffs)
      assert "+A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # " == delta

      # Convert delta string into a diff.
      assert diffs == Diff.from_delta("", delta)
    end

    test "160 kb string" do
      a = Enum.reduce(1..14, "abcdefghij", fn _, str -> str <> str end)

      diffs = [{:insert, a}]
      delta = Diff.to_delta(diffs)
      assert "+" <> a == delta

      # Convert delta string into a diff.
      assert diffs == Diff.from_delta("", delta)
    end
  end

  describe "x_index" do
    test "translation on insertion" do
      assert 5 == Diff.x_index([{:delete, "a"}, {:insert, "1234"}, {:equal, "xyz"}], 2)
    end

    test "translation on deletion" do
      assert 1 == Diff.x_index([{:equal, "a"}, {:delete, "1234"}, {:equal, "xyz"}], 3)
    end
  end

  describe "levenshtein" do
    test "levenshtein with trailing equality" do
      assert 4 == Diff.levenshtein([{:delete, "abc"}, {:insert, "1234"}, {:equal, "xyz"}])
    end

    test "levenshtein with leading equality" do
      assert 4 == Diff.levenshtein([{:equal, "xyz"}, {:delete, "abc"}, {:insert, "1234"}])
    end

    test "levenshtein with middle equality" do
      assert 7 == Diff.levenshtein([{:delete, "abc"}, {:equal, "xyz"}, {:insert, "1234"}])
    end
  end

  describe "bisect" do
    test "normal" do
      # Since the resulting diff hasn't been normalized, it would be ok if
      # the insertion and deletion pairs are swapped.
      # If the order changes, tweak this test as required.
      assert [{:delete, "c"}, {:insert, "m"}, {:equal, "a"}, {:delete, "t"}, {:insert, "p"}] ==
               Diff.bisect("cat", "map", one_second())
    end

    test "zero timeout" do
      assert [{:delete, "cat"}, {:insert, "map"}] == Diff.bisect("cat", "map", 0)
    end
  end

  describe "main" do
    test "null case" do
      assert [] == Diff.main("", "", false)
    end

    test "equality" do
      assert [{:equal, "abc"}] == Diff.main("abc", "abc", false)
    end

    test "simple insertion" do
      assert [{:equal, "ab"}, {:insert, "123"}, {:equal, "c"}] ==
               Diff.main("abc", "ab123c", false)
    end

    test "simple deletion" do
      assert [{:equal, "a"}, {:delete, "123"}, {:equal, "bc"}] ==
               Diff.main("a123bc", "abc", false)
    end

    test "two insertions" do
      assert [{:equal, "a"}, {:insert, "123"}, {:equal, "b"}, {:insert, "456"}, {:equal, "c"}] ==
               Diff.main("abc", "a123b456c", false)
    end

    test "two deletions" do
      assert [{:equal, "a"}, {:delete, "123"}, {:equal, "b"}, {:delete, "456"}, {:equal, "c"}] ==
               Diff.main("a123b456c", "abc", false)
    end

    # Perform a real diff.
    # Switch off the timeout.

    test "simple case 1" do
      assert [{:delete, "a"}, {:insert, "b"}] == Diff.main("a", "b", false, with_timeout(0))
    end

    test "simple case 2" do
      assert [
               {:delete, "Apple"},
               {:insert, "Banana"},
               {:equal, "s are a"},
               {:insert, "lso"},
               {:equal, " fruit."}
             ] ==
               Diff.main(
                 "Apples are a fruit.",
                 "Bananas are also fruit.",
                 false,
                 with_timeout(0)
               )
    end

    test "simple case 3" do
      assert [
               {:delete, "a"},
               {:insert, "\u0680"},
               {:equal, "x"},
               {:delete, "\t"},
               {:insert, "\x00"}
             ] == Diff.main("ax\t", "\u0680x\x00", false, with_timeout(0))
    end

    test "overlap 1" do
      assert [
               {:delete, "1"},
               {:equal, "a"},
               {:delete, "y"},
               {:equal, "b"},
               {:delete, "2"},
               {:insert, "xab"}
             ] == Diff.main("1ayb2", "abxab", false, with_timeout(0))
    end

    test "overlap 2" do
      assert [{:insert, "xaxcx"}, {:equal, "abc"}, {:delete, "y"}] ==
               Diff.main("abcy", "xaxcxabc", false, with_timeout(0))
    end

    test "overlap 3" do
      assert [
               {:delete, "ABCD"},
               {:equal, "a"},
               {:delete, "="},
               {:insert, "-"},
               {:equal, "bcd"},
               {:delete, "="},
               {:insert, "-"},
               {:equal, "efghijklmnopqrs"},
               {:delete, "EFGHIJKLMNOefg"}
             ] ==
               Diff.main(
                 "ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg",
                 "a-bcd-efghijklmnopqrs",
                 false,
                 with_timeout(0)
               )
    end

    test "large equality" do
      assert [
               {:insert, " "},
               {:equal, "a"},
               {:insert, "nd"},
               {:equal, " [[Pennsylvania]]"},
               {:delete, " and [[New"}
             ] ==
               Diff.main(
                 "a [[Pennsylvania]] and [[New",
                 " and [[Pennsylvania]]",
                 false,
                 with_timeout(0)
               )
    end
  end

  test "timeout" do
    # 100 ms
    timeout = 0.1

    a =
      "`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n"

    b =
      "I am the very model of a modern major general,\nI've information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n"

    # Increase the text lengths by 1024 times to ensure a timeout.
    {a, b} = Enum.reduce(1..10, {a, b}, fn _, {a, b} -> {a <> a, b <> b} end)
    start_time = :os.system_time(:millisecond)
    Diff.main(a, b, with_timeout(timeout))
    end_time = :os.system_time(:millisecond)
    # Test that we took at least the timeout period.
    assert timeout * 1_000 <= end_time - start_time
    # Test that we didn't take forever (be forgiving).
    # Theoretically this test could fail very occasionally if the
    # OS task swaps or locks up for a second at the wrong moment.
    assert timeout * 30_000 > end_time - start_time
  end

  # Test the linemode speedup.
  # Must be long to pass the 100 char cutoff.
  describe "line mode" do
    test "simple" do
      a = String.duplicate("1234567890\n", 13)
      b = String.duplicate("abcdefghij\n", 13)
      assert Diff.main(a, b, false) == Diff.main(a, b, true)
    end

    test "single" do
      a = String.duplicate("1234567890", 13)
      b = String.duplicate("abcdefghij", 13)
      assert Diff.main(a, b, false) == Diff.main(a, b, true)
    end

    test "overlap" do
      a = String.duplicate("1234567890\n", 13)

      b =
        "abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n"

      texts_linemode = Diff.main(a, b, true) |> rebuild_texts()
      texts_textmode = Diff.main(a, b, false) |> rebuild_texts()
      assert texts_textmode == texts_linemode
    end
  end
end
