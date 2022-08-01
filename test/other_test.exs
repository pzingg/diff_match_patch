defmodule OtherTest do
  use ExUnit.Case

  alias Dmp.{Match, Options, StringUtils}

  doctest Dmp.Options
  doctest Dmp.StringUtils

  describe "bitap scores" do
    test "wu manber exact" do
      assert 6 == Match.bitap("aabaacaabacab", "aabac", 0, 0.4, 100)
    end

    test "wu manber 1 error" do
      assert 6 == Match.bitap("aabaacabbacab", "aabac", 6, 0.4, 100)
    end
  end
end
