defmodule ElvenGardTowerTest do
  use ExUnit.Case
  doctest ElvenGardCitadel

  test "greets the world" do
    assert ElvenGardCitadel.hello() == :world
  end
end
