defmodule Chiaroscuro.MixProject do
  use Mix.Project

  def project do
    [
      app: :chiaroscuro,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [format: ["format", &zig_format/1]],
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps,
    do: [
      # "~> 0.7", only: :test}
      {:zigler, path: "../zigler", only: :test}
    ]

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp zig_format(_) do
    files = get_recursive_files("src")
    System.cmd("zig", ["fmt" | files])
  end

  def get_recursive_files(dir) do
    dir
    |> File.ls!()
    |> Enum.flat_map(fn file ->
      path = Path.join(dir, file)

      cond do
        File.dir?(path) ->
          get_recursive_files(path)

        Path.extname(path) == ".zig" ->
          [path]

        true ->
          []
      end
    end)
  end
end
