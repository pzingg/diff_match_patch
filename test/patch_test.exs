defmodule PatchTest do
  use ExUnit.Case

  alias Dmp.{Diff, Options, Patch}

  # doctest Dmp.Patch

  defp patch_fixture_1() do
    {"The quick brown fox jumps over the lazy dog.",
     "That quick brown fox jumped over a lazy dog."}
  end

  defp patch_fixture_2() do
    {text1, text2} = patch_fixture_1()

    expected_patch =
      "@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n"

    {text1, text2, expected_patch}
  end

  defp patch_fixture_3() do
    Patch.make(
      "The quick brown fox jumps over the lazy dog.",
      "That quick brown fox jumped over a lazy dog."
    )
  end

  defp with_patch_delete_threshold(t) do
    opts = Options.default()
    %Options{opts | patch_delete_threshold: t}
  end

  defp with_zero_match_threshold_and_distance() do
    opts = Options.default()
    %Options{opts | match_threshold: 0, match_distance: 0}
  end

  test "Patch object" do
    p = %Patch{
      start1: 20,
      start2: 21,
      length1: 18,
      length2: 17,
      diffs: [
        {:equal, "jump"},
        {:delete, "s"},
        {:insert, "ed"},
        {:equal, " over "},
        {:delete, "the"},
        {:insert, "a"},
        {:equal, "\nlaz"}
      ]
    }

    strp = to_string(p)
    assert "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n" = strp
  end

  describe "from_text" do
    test "null case" do
      assert [] == Patch.from_text("")
    end

    test "example 1" do
      strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n"
      assert strp == Patch.from_text(strp) |> List.first() |> to_string()
    end

    test "example 2" do
      assert "@@ -1 +1 @@\n-a\n+b\n" ==
               Patch.from_text("@@ -1 +1 @@\n-a\n+b\n") |> List.first() |> to_string()
    end

    test "example 3" do
      assert "@@ -1,3 +0,0 @@\n-abc\n" ==
               Patch.from_text("@@ -1,3 +0,0 @@\n-abc\n") |> List.first() |> to_string()
    end

    test "example 4" do
      assert "@@ -0,0 +1,3 @@\n+abc\n" ==
               Patch.from_text("@@ -0,0 +1,3 @@\n+abc\n") |> List.first() |> to_string()
    end

    test "somersault" do
      p =
        Patch.from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")
        |> List.first()

      assert %Patch{
               diffs: [{:delete, "jump"}, {:insert, "somersault"}],
               length1: 4,
               length2: 10,
               start1: 20,
               start2: 20
             } == p
    end

    test "generates error" do
      assert_raise RuntimeError, "Invalid patch header: Bad", fn ->
        Patch.from_text("Bad\nPatch\n")
      end
    end
  end

  describe "to_text" do
    test "example 1" do
      strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n"
      p = Patch.from_text(strp)
      assert strp == Patch.to_text(p)
    end

    test "example 2" do
      strp = "@@ -1,9 +1,9 @@\n-f\n+F\n oo+fooba\n@@ -7,9 +7,9 @@\n obar\n-,\n+.\n tes\n"
      p = Patch.from_text(strp)
      assert strp == Patch.to_text(p)
    end
  end

  describe "add_context" do
    test "example 1" do
      p =
        Patch.from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")
        |> List.first()
        |> Patch.add_context("The quick brown fox jumps over the lazy dog.", 4)

      assert "@@ -17,12 +17,18 @@\n fox \n-jump\n+somersault\n s ov\n" == to_string(p)
    end

    test "not enough trailing context" do
      p =
        Patch.from_text("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")
        |> List.first()
        |> Patch.add_context("The quick brown fox jumps.", 4)

      assert "@@ -17,10 +17,16 @@\n fox \n-jump\n+somersault\n s.\n" == to_string(p)
    end

    test "not enough leading context" do
      p =
        Patch.from_text("@@ -3 +3,2 @@\n-e\n+at\n")
        |> List.first()
        |> Patch.add_context("The quick brown fox jumps.", 4)

      assert "@@ -1,7 +1,8 @@\n Th\n-e\n+at\n  qui\n" == to_string(p)
    end

    test "with ambiguity" do
      p =
        Patch.from_text("@@ -3 +3,2 @@\n-e\n+at\n")
        |> List.first()
        |> Patch.add_context("The quick brown fox jumps.  The quick brown fox crashes.", 4)

      assert "@@ -1,27 +1,28 @@\n Th\n-e\n+at\n  quick brown fox jumps. \n" == to_string(p)
    end
  end

  describe "make" do
    test "null case" do
      patches = Patch.make("", "")
      assert "" == Patch.to_text(patches)
    end

    test "text2 + text1 inputs" do
      {text1, text2} = patch_fixture_1()
      # The second patch must be "-21,17 +21,18", not "-22,17 +21,18" due to rolling context.
      expected_patch =
        "@@ -1,8 +1,7 @@\n Th\n-at\n+e\n  qui\n@@ -21,17 +21,18 @@\n jump\n-ed\n+s\n  over \n-a\n+the\n  laz\n"

      patches = Patch.make(text2, text1)
      assert expected_patch == Patch.to_text(patches)
    end

    test "text1 + text2 inputs" do
      {text1, text2, expected_patch} = patch_fixture_2()
      patches = Patch.make(text1, text2)
      assert expected_patch == Patch.to_text(patches)
    end

    test "diff input" do
      {text1, text2, expected_patch} = patch_fixture_2()
      diffs = Diff.main(text1, text2, false)
      patches = Patch.from_diffs(diffs)
      assert expected_patch == Patch.to_text(patches)
    end

    test "text1 + diff inputs" do
      {text1, text2, expected_patch} = patch_fixture_2()
      diffs = Diff.main(text1, text2, false)
      patches = Patch.make(text1, diffs)
      assert expected_patch == Patch.to_text(patches)
    end

    test "text1 + text2 + diff inputs (deprecated)" do
      {text1, text2, expected_patch} = patch_fixture_2()
      diffs = Diff.main(text1, text2, false)
      patches = Patch.from_texts_and_diffs(text1, text2, diffs)
      assert expected_patch == Patch.to_text(patches)
    end

    test "character encoding" do
      patches = Patch.make("`1234567890-=[]\\;',./", "~!@#$%^&*()_+{}|:\"<>?")

      assert "@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n" ==
               Patch.to_text(patches)
    end

    test "character decoding" do
      diffs = [{:delete, "`1234567890-=[]\\;',./"}, {:insert, "~!@#$%^&*()_+{}|:\"<>?"}]

      %Patch{diffs: patch_diffs} =
        Patch.from_text(
          "@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n"
        )
        |> List.first()

      assert diffs == patch_diffs
    end

    @tag :skip
    test "long string with repeats" do
      text1 = String.duplicate("abcdef", 100)
      text2 = text1 <> "123"
      expected_patch = "@@ -573,28 +573,31 @@\n cdefabcdefabcdefabcdefabcdef\n+123\n"
      patches = Patch.make(text1, text2)
      assert expected_patch == Patch.to_text(patches)
    end
  end

  describe "split_max" do
    # Assumes that Match_MaxBits is 32.
    test "example 1" do
      patches =
        Patch.make(
          "abcdefghijklmnopqrstuvwxyz01234567890",
          "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0"
        )

      patches = Patch.split_max(patches, 4)
      assert patches != []

      assert "@@ -1,32 +1,46 @@\n+X\n ab\n+X\n cd\n+X\n ef\n+X\n gh\n+X\n ij\n+X\n kl\n+X\n mn\n+X\n op\n+X\n qr\n+X\n st\n+X\n uv\n+X\n wx\n+X\n yz\n+X\n 012345\n@@ -25,13 +39,18 @@\n zX01\n+X\n 23\n+X\n 45\n+X\n 67\n+X\n 89\n+X\n 0\n" ==
               Patch.to_text(patches)
    end

    test "example 2" do
      patches =
        Patch.make(
          "abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz",
          "abcdefuvwxyz"
        )

      old_to_text = Patch.to_text(patches)
      patches = Patch.split_max(patches, 4)
      assert old_to_text == Patch.to_text(patches)
    end

    test "example 3" do
      patches =
        Patch.make(
          "1234567890123456789012345678901234567890123456789012345678901234567890",
          "abc"
        )

      patches = Patch.split_max(patches, 4)

      assert "@@ -1,32 +1,4 @@\n-1234567890123456789012345678\n 9012\n@@ -29,32 +1,4 @@\n-9012345678901234567890123456\n 7890\n@@ -57,14 +1,3 @@\n-78901234567890\n+abc\n" ==
               Patch.to_text(patches)
    end

    @tag :skip
    test "example 4" do
      patches =
        Patch.make(
          "abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1",
          "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1"
        )

      patches = Patch.split_max(patches, 4)

      assert "@@ -2,32 +2,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n@@ -29,32 +29,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n" ==
               Patch.to_text(patches)
    end
  end

  describe "add_padding" do
    test "both edges full" do
      patches = Patch.make("", "test")
      assert "@@ -0,0 +1,4 @@\n+test\n" == Patch.to_text(patches)
      {patches, padding} = Patch.add_padding(patches, 4)
      assert "\x01\x02\x03\x04" == padding
      assert "@@ -1,8 +1,12 @@\n %01%02%03%04\n+test\n %01%02%03%04\n" == Patch.to_text(patches)
    end

    test "both edges partial" do
      patches = Patch.make("XY", "XtestY")
      assert "@@ -1,2 +1,6 @@\n X\n+test\n Y\n" == Patch.to_text(patches)
      {patches, padding} = Patch.add_padding(patches, 4)
      assert "\x01\x02\x03\x04" == padding
      assert "@@ -2,8 +2,12 @@\n %02%03%04X\n+test\n Y%01%02%03\n" == Patch.to_text(patches)
    end

    test "both edges none" do
      patches = Patch.make("XXXXYYYY", "XXXXtestYYYY")
      assert "@@ -1,8 +1,12 @@\n XXXX\n+test\n YYYY\n" == Patch.to_text(patches)
      {patches, padding} = Patch.add_padding(patches, 4)
      assert "\x01\x02\x03\x04" == padding
      assert "@@ -5,8 +5,12 @@\n XXXX\n+test\n YYYY\n" == Patch.to_text(patches)
    end
  end

  describe "apply" do
    test "null case" do
      patches = Patch.make("", "")
      results = Patch.apply(patches, "Hello world.")
      assert {"Hello world.", []} == results
    end

    test "exact match" do
      patches = patch_fixture_3()
      results = Patch.apply(patches, "The quick brown fox jumps over the lazy dog.")
      assert {"That quick brown fox jumped over a lazy dog.", [true, true]} == results
    end

    @tag :skip
    test "partial match" do
      patches = patch_fixture_3()
      results = Patch.apply(patches, "The quick red rabbit jumps over the tired tiger.")
      assert {"That quick red rabbit jumped over a tired tiger.", [true, true]} == results
    end

    test "failed match" do
      patches = patch_fixture_3()
      results = Patch.apply(patches, "I am the very model of a modern major general.")
      assert {"I am the very model of a modern major general.", [false, false]} == results
    end

    @tag :skip
    test "big delete, small change" do
      patches =
        Patch.make(
          "x1234567890123456789012345678901234567890123456789012345678901234567890y",
          "xabcy"
        )

      results =
        Patch.apply(
          patches,
          "x123456789012345678901234567890-----++++++++++-----123456789012345678901234567890y"
        )

      assert {"xabcy", [true, true]} == results
    end

    @tag :skip
    test "big delete, big change 1" do
      patches =
        Patch.make(
          "x1234567890123456789012345678901234567890123456789012345678901234567890y",
          "xabcy"
        )

      results =
        Patch.apply(
          patches,
          "x12345678901234567890---------------++++++++++---------------12345678901234567890y"
        )

      assert {"xabc12345678901234567890---------------++++++++++---------------12345678901234567890y",
              [false, true]} == results
    end

    @tag :skip
    test "big delete, big change 2" do
      opts = with_patch_delete_threshold(0.6)

      patches =
        Patch.make(
          "x1234567890123456789012345678901234567890123456789012345678901234567890y",
          "xabcy",
          opts
        )

      results =
        Patch.apply(
          patches,
          "x12345678901234567890---------------++++++++++---------------12345678901234567890y",
          opts
        )

      assert {"xabcy", [true, true]} == results
    end

    @tag :skip
    test "compensate for failed patch" do
      opts = with_zero_match_threshold_and_distance()

      patches =
        Patch.make(
          "abcdefghijklmnopqrstuvwxyz--------------------1234567890",
          "abcXXXXXXXXXXdefghijklmnopqrstuvwxyz--------------------1234567YYYYYYYYYY890",
          opts
        )

      results =
        Patch.apply(patches, "ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567890", opts)

      assert {"ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567YYYYYYYYYY890", [false, true]} ==
               results
    end

    @tag :skip
    test "no side effects" do
      patches = Patch.make("", "test")
      patchstr = Patch.to_text(patches)
      results = Patch.apply(patches, "")
      assert patchstr == Patch.to_text(patches)
    end

    @tag :skip
    test "no side effects with major delete" do
      patches = Patch.make("The quick brown fox jumps over the lazy dog.", "Woof")
      patchstr = Patch.to_text(patches)
      results = Patch.apply(patches, "The quick brown fox jumps over the lazy dog.")
      assert patchstr == Patch.to_text(patches)
    end

    @tag :skip
    test "edge exact match" do
      patches = Patch.make("", "test")
      results = Patch.apply(patches, "")
      assert {"test", [true]} == results
    end

    @tag :skip
    test "near edge exact match" do
      patches = Patch.make("XY", "XtestY")
      results = Patch.apply(patches, "XY")
      assert {"XtestY", [true]} == results
    end

    @tag :skip
    test "edge partial match" do
      patches = Patch.make("y", "y123")
      results = Patch.apply(patches, "x")
      assert {"x123", [true]} == results
    end
  end
end
