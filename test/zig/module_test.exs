defmodule CharoscuroTest.Zig.ModuleTest do
  use ExUnit.Case, async: true

  # TODO: drive these tests from inside of elixir using Zigler.
  @module_harness Path.join(__DIR__, "module_harness.zig")

  test "the module harness" do
    case System.cmd("zig", ["test", @module_harness], stderr_to_stdout: true) do
      {_, 0} ->
        :ok

      {err, _} ->
        raise "module harness test error\n: #{err}"
    end
  end
end
