defmodule :console do
  def log(content) do
    content
    |> inspect
    |> IO.puts
  end
end
