defmodule Basic do
  def foo, do: bar()

  defp bar, do: "bar"

  def l, do: &bar/0

  def m, do: -1
end
