defmodule Chiaroscuro do
  defmacro serve_modules(list, _opts \\ []) do
    # consider searching dependencies, too.
    plugs = list
    |> Enum.map(fn module_ast ->
      module = Macro.expand(module_ast, __CALLER__)
      unless match?({:module, ^module}, Code.ensure_compiled(module)) do
        raise CompileError,
          line: __CALLER__.line,
          file: __CALLER__.file,
          description: "#{inspect module} does not exist"
      end

      case :code.get_object_code(module) do
        {^module, _binary, path} ->
          {appname(path), Path.basename(path)}
      end
    end)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(&static_plug_for/1)

    quote do
      unquote_splicing(plugs)

      # TODO: figure out gzip serving.
      plug Plug.Static,
        at: "/wasm",
        from: {:chiaroscuro, "priv/wasm"},
        gzip: false,
        only: ~w(chiaroscuro.wasm)

      # TODO: make this dynamic.
      plug Plug.Static,
        at: "/js",
        from: {:chiaroscuro, "priv/js"},
        gzip: false,
        only: ~w(chiaroscuro.js)
    end
  end

  defp appname(path) do
    path
    |> Path.dirname
    |> Path.dirname
    |> Path.basename
    |> String.to_atom
  end

  defp static_plug_for({app, files}) do
    quote bind_quoted: [app: app, files: files] do
      plug Plug.Static,
        at: "/modules",
        from: {app, "ebin"},
        gzip: false,
        only: files
    end
  end
end
