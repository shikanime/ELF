defmodule ElvenGardGateTest do
  use ExUnit.Case
  doctest ElvenGardGate

  test "greets the world" do
    assert ElvenGardGate.hello() == :world
  end
end
