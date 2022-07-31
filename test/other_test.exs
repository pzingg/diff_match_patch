defmodule OtherTest do
  use ExUnit.Case

  alias Dmp.{Match, Options, StringUtils}

  doctest Dmp.Options
  doctest Dmp.StringUtils

  describe "bitap scores" do
    test "inexact match" do
      {loc, score} = Match.bitap("abcdefghijk", "efxyhi", 1, 0.4, 100, true)
      assert 4 == loc
      assert_in_delta 0.36333, score, 0.00001
    end
  end
end
