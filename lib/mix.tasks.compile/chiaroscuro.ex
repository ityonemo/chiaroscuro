defmodule Mix.Tasks.Compile.Chiaroscuro do
  use Mix.Task

  # TODO: figure out if the end-user has local zig language, pull it down
  # if need be.

  @src_dir __DIR__
  |> Path.join("../../src")
  |> Path.expand

  @wasm_dir :chiaroscuro
  |> :code.priv_dir
  |> Path.join("wasm")

  def run(_) do

    chiaroscuro_src = Path.join(@src_dir, "chiaroscuro.zig")
    chiaroscuro_dst = Path.join(@wasm_dir, "chiaroscuro.wasm")

    case System.cmd("zig", ~w(
      build-lib
      -target wasm32-freestanding
      -O ReleaseSmall
      #{chiaroscuro_src}),
      stderr_to_stdout: true) do

      {_, 0} -> :ok
      {error, _} ->
        Mix.raise("error compiling chiaroscuro with #{error}")
    end
    File.cp!("chiaroscuro.wasm", chiaroscuro_dst)
    File.rm!("chiaroscuro.wasm")
    :ok
  end
end
