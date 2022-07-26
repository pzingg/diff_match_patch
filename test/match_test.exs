defmodule MatchTest do
  use ExUnit.Case

  alias Dmp.{Match, Options}

  # doctest Dmp.Match

  describe "alphabet" do
    test "abc" do
      assert %{?a => 4, ?b => 2, ?c => 1} = Match.alphabet("abc")
    end

    test "abcaba" do
      assert %{?a => 37, ?b => 18, ?c => 8} == Match.alphabet("abcaba")
    end
  end
end
