defmodule ZigTest do
  use ExUnit.Case

  @moduledoc "tests that all of the zig code is sound"

  test "the zig code's internal tests" do
    assert {_, 0} =
      System.cmd("zig", ["test", "src/chiaroscuro.zig"])
  end
end
