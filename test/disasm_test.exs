defmodule ChiaroscuroTest.DisasmTest do
  # do I actually understand how to disassemble BEAM modules?

  use ExUnit.Case, async: true

  test "let's disassemble a test module" do
    {_, b, _} = :code.get_object_code(Basic)
    assert :beam_disasm.file(b) == Disasm.bin(b)
  end

  test "let's dissasemble everything" do
    :code.all_loaded()
    |> Enum.map(fn {module, _} ->
      case :code.get_object_code(module) do
        {_, module_bin, _} ->
          IO.inspect(module)
          assert :beam_disasm.file(module_bin) == Disasm.bin(module_bin)
        :error -> :ok
      end
    end)
  end
end
