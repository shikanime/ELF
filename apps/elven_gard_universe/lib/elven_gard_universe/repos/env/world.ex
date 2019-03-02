defmodule ElvenGardUniverse.Env.World do
  use Ecto.Schema
  import Ecto.Changeset

  schema "worlds" do
    field :name

    has_many :channels, ElvenGardUniverse.Env.Channel
    has_many :maps, ElvenGardUniverse.Env.Channel

    timestamps()
  end

  @doc false
  def changeset(world, attrs) do
    world
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
