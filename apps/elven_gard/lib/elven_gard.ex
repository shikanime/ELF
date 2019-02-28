defmodule ElvenGard do
  def sync_hero(id) do
    Swarm.whereis_or_register_name(id, ElvenGard.EntitesPool, :register, [])
  end
end
