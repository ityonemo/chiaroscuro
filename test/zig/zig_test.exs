defmodule ChiaroscuroTest.ZigTest do
  use ExUnit.Case

  @moduledoc "tests that all of the zig code is sound"

  test "the zig code's internal tests" do
    case System.cmd("zig", ["test", "src/chiaroscuro.zig"], stderr_to_stdout: true) do
      {_, 0} ->
        :ok

      {err, _} ->
        raise "internal test error\n: #{err}"
    end
  end
end
