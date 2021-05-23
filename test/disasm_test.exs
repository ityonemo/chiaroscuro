defmodule ChiaroscuroTest.DisasmTest do
  # do I actually understand how to disassemble BEAM modules?

  use ExUnit.Case, async: true

  test "let's disassemble a test module" do
    {_, b, _} = :code.get_object_code(Basic)
    assert :beam_disasm.file(b) == Disasm.bin(b)
  end

  @incomplete_functions [Mix.Hex, :raw_file_io_raw, Enumerable, :counters, Mix.Task.Compiler,
  :dtls_server_session_cache_sup, :dtls_listener_sup, :inets_app, Mix.Dep.ElixirSCM, Hex.Set,
  :beam_opcodes, :erl_tracer, String.Chars.Atom, Supervisor.Default, Agent, Process, :erl_bifs,
  Mix.RemoteConverger, :httpc_handler_sup, :logger_sup, Mix.SCM, :persistent_term, Mix.SCM.Path,
  Mix, List.Chars.BitString, Inspect, String.Chars, :erl_signal_handler, Logger.Filter,
  Logger.Counter, Kernel.ErrorHandler, :dtls_connection_sup, :tls_connection_sup, :elixir_sup,
  String.Chars.Integer, :otp_internal, Mix.Tasks.Deps.Precompile]
  @bad_exports [:erl_eval, :epp, :file, :inet_parse, :erl_lint, :logger_handler_watcher, :user]
  @unk_100111 [:beam_lib, :io_lib_format, :gb_sets, :inet_db, :compile, :rand, Float,
  ExUnit.CLIFormatter, Kernel, String]
  @still_buggy @incomplete_functions ++ @bad_exports ++ @unk_100111

  test "let's dissasemble everything" do
    :code.all_loaded()
    |> Enum.reject(&(elem(&1, 0) in @still_buggy)) # modules that have wierd issues
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

    assert exports == exports2, "bad exports: #{inspect module}"
    assert vsn == vsn
    assert attrs == attrs2

    assert length(funs) == length(funs2), "mismatch funs count: #{inspect module}"
    funs
    |> Enum.zip(funs2)
    |> Enum.map(fn {fun, fun2} ->
      assert fun == fun2
    end)
  end

  @tag :skip
  test "compare one module" do
    module_bin = Float
    |> :code.get_object_code
    |> elem(1)

    assert :beam_disasm.file(module_bin) == Disasm.bin(module_bin)
  end
end
