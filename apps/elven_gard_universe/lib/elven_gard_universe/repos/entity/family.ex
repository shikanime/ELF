defmodule ElvenGardUniverse.Entity.Family do
  use Ecto.Schema
  import Ecto.Changeset

  schema "families" do
    field :name
    field :level

    belongs_to :hero, ElvenGardUniverse.Entity.Hero

    timestamps()
  end

  @doc false
  def changeset(entity, attrs) do
    entity
    |> cast(attrs, [:name, :x, :y, :z])
    |> validate_required([:name, :x, :y, :z])
  end
end
