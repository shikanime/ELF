defmodule ElvenGardUniverse.Society.Class do
  use Ecto.Schema
  import Ecto.Changeset

  schema "classes" do
    field :name

    has_many :characters, ElvenGardUniverse.Entity.Character

    timestamps()
  end

  @doc false
  def changeset(class, attrs) do
    class
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
