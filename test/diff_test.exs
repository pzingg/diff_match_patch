defmodule DiffTest do
  use ExUnit.Case

  alias Dmp.Diff

  # doctest Dmp.Diff

  defp one_second() do
    :os.system_time(:millisecond) + 1_000
  end

  defp rebuild_texts(diffs) do
    # Construct the two texts which made up the diff originally.
    text1 = ""
    text2 = ""

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
      assert nil == Diff.half_match("1234567890", "abcdef", one_second)
    end

    test "no match 2" do
      assert nil == Diff.half_match("12345", "23", one_second)
    end

    test "single match 1" do
      assert {"12", "90", "a", "z", "345678"} ==
               Diff.half_match("1234567890", "a345678z", one_second)
    end

    test "single match 2" do
      assert {"a", "z", "12", "90", "345678"} ==
               Diff.half_match("a345678z", "1234567890", one_second)
    end

    test "single match 3" do
      assert {"abc", "z", "1234", "0", "56789"} ==
               Diff.half_match("abc56789z", "1234567890", one_second)
    end

    test "single match 4" do
      assert {"a", "xyz", "1", "7890", "23456"} ==
               Diff.half_match("a23456xyz", "1234567890", one_second)
    end

    @tag :good
    test "mulitple match 1" do
      assert {"12123", "123121", "a", "z", "1234123451234"} ==
               Diff.half_match("121231234123451234123121", "a1234123451234z", one_second)
    end

    test "multiple match 2" do
      assert {"", "-=-=-=-=-=", "x", "", "x-=-=-=-=-=-=-="} ==
               Diff.half_match("x-=-=-=-=-=-=-=-=-=-=-=-=", "xx-=-=-=-=-=-=-=", one_second)
    end

    test "multiple match 3" do
      assert {"-=-=-=-=-=", "", "", "y", "-=-=-=-=-=-=-=y"} ==
               Diff.half_match("-=-=-=-=-=-=-=-=-=-=-=-=y", "-=-=-=-=-=-=-=yy", one_second)
    end
  end
end
