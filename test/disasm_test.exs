defmodule ChiaroscuroTest.DisasmTest do
  # do I actually understand how to disassemble BEAM modules?

  use ExUnit.Case, async: true

  test "let's disassemble a test module" do
    {_, b, _} = :code.get_object_code(Basic)
    assert :beam_disasm.file(b) == Disasm.bin(b)
  end

  @funny_modules [:otp_internal, :erl_eval, Mix.Hex, :raw_file_io_raw, Enumerable]

  test "let's dissasemble everything" do
    :code.all_loaded()
    |> Enum.reject(&(elem(&1, 0) in @funny_modules)) # modules that have wierd issues
    |> Enum.each(fn {module, _} ->
      case :code.get_object_code(module) do
        {_, module_bin, _} ->
          compare(module_bin)
        :error -> :ok
      end
    end)
  end

  defp compare(module_bin) do
    assert {:beam_file, module, exports, vsn, attrs, funs} = :beam_disasm.file(module_bin)
    assert {:beam_file, ^module, exports2, vsn2, attrs2, funs2} = Disasm.bin(module_bin, module: module)

    assert exports == exports2
    assert vsn == vsn
    assert attrs == attrs2

    assert length(funs) == length(funs2)
    funs
    |> Enum.zip(funs2)
    |> Enum.map(fn {fun, fun2} ->
      assert fun == fun2
    end)
  end
end
