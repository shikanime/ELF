defmodule ElvenGardUniverse.Entity.Family do
  use Ecto.Schema
  import Ecto.Changeset

  schema "families" do
    field :name
    field :level

    belongs_to :Character, ElvenGardUniverse.Entity.Character

    timestamps()
  end

  @doc false
  def changeset(entity, attrs) do
    entity
    |> cast(attrs, [:name, :x, :y, :z])
    |> validate_required([:name, :x, :y, :z])
  end
end
