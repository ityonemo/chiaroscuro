defmodule ChiaroscuroTest.DisasmTest do
  # do I actually understand how to disassemble BEAM modules?

  use ExUnit.Case, async: true

  test "let's dissasemble everything" do
    {_, module_binary, _} = :code.get_object_code(Basic)
    assert :beam_disasm.file(module_binary) == Disasm.bin(module_binary)
  end
end
