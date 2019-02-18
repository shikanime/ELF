defmodule ElvenGardAuthTest do
  use ExUnit.Case
  doctest ElvenGardAuth

  test "greets the world" do
    assert ElvenGardAuth.hello() == :world
  end
end
