defmodule MatchTest do
  use ExUnit.Case

  alias Dmp.Match

  doctest Dmp.Match

  describe "alphabet" do
    test "abc" do
      assert %{?a => 4, ?b => 2, ?c => 1} = Match.alphabet("abc")
    end

    test "abcaba" do
      assert %{?a => 37, ?b => 18, ?c => 8} == Match.alphabet("abcaba")
    end
  end

  describe "bitap" do
    test "exact match 1" do
      assert 5 == Match.bitap("abcdefghijk", "fgh", 5, 0.5, 100)
    end

    test "exact match 2" do
      assert 5 == Match.bitap("abcdefghijk", "fgh", 0, 0.5, 100)
    end

    test "fuzzy match 1" do
      assert 4 == Match.bitap("abcdefghijk", "efxhi", 0, 0.5, 100)
    end

    test "fuzzy match 2" do
      assert 2 == Match.bitap("abcdefghijk", "cdefxyhijk", 5, 0.5, 100)
    end

    test "fuzzy match 3" do
      assert -1 == Match.bitap("abcdefghijk", "bxy", 1, 0.5, 100)
    end

    test "overflow 1" do
      assert 2 == Match.bitap("123456789xx0", "3456789x0", 2, 0.5, 100)
    end

    test "overflow 2" do
      assert 0 == Match.bitap("abcdef", "xxabc", 4, 0.5, 100)
    end

    test "overflow 3" do
      assert 3 == Match.bitap("abcdef", "defyy", 4, 0.5, 100)
    end

    test "overflow 4" do
      assert 0 == Match.bitap("abcdef", "xabcdefy", 0, 0.5, 100)
    end

    test "threshold 0.4" do
      assert 4 == Match.bitap("abcdefghijk", "efxyhi", 1, 0.4, 100)
    end

    test "threshold 0.3" do
      assert -1 == Match.bitap("abcdefghijk", "efxyhi", 1, 0.3, 100)
    end

    test "threshold 0" do
      assert 1 == Match.bitap("abcdefghijk", "bcdef", 1, 0, 100)
    end

    test "multiple select 1" do
      assert 0 == Match.bitap("abcdexyzabcde", "abccde", 3, 0.5, 100)
    end

    test "multiple select 2" do
      assert 8 == Match.bitap("abcdexyzabcde", "abccde", 5, 0.5, 100)
    end

    test "multiple select loop" do
      text = "abcdexyzabcde"

      results =
        Enum.map(0..(String.length(text) - 1), fn loc ->
          Match.bitap("abcdexyzabcde", "abccde", loc, 0.5, 100)
        end)

      assert [0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8] == results
    end

    test "strict distance 1" do
      assert -1 == Match.bitap("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24, 0.5, 10)
    end

    test "strict distance 2" do
      assert 0 == Match.bitap("abcdefghijklmnopqrstuvwxyz", "abcdxxefg", 1, 0.5, 10)
    end

    test "loose distance" do
      assert 0 == Match.bitap("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24, 0.5, 1000)
    end
  end

  describe "main" do
    test "shortcut match 1" do
      assert 0 == Match.main("abcdef", "abcdef", 1000)
    end

    test "shortcut match 2" do
      assert -1 == Match.main("", "abcdef", 1)
    end

    test "shortcut match 3" do
      assert 3 == Match.main("abcdef", "", 3)
    end

    test "shortcut match 4" do
      assert 3 == Match.main("abcdef", "de", 3)
    end

    test "shortcut match 5" do
      assert 3 == Match.main("abcdef", "defy", 4)
    end

    test "shortcut match 6" do
      assert 0 == Match.main("abcdef", "abcdefy", 0)
    end

    test "complex match" do
      assert 4 ==
               Match.main(
                 "I am the very model of a modern major general.",
                 " that berry ",
                 5,
                 match_threshold: 0.7
               )
    end
  end
end
