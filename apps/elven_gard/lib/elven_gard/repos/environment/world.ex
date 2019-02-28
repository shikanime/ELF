defmodule ElvenGard.Environment.World do
  use Ecto.Schema
  import Ecto.Changeset

  schema "worlds" do
    field :name

    has_many :channels, ElvenGard.Environment.Channel
    has_many :maps, ElvenGard.Environment.Channel

    timestamps()
  end

  @doc false
  def changeset(world, attrs) do
    world
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
