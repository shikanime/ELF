defmodule ElvenGardUniverse.Env.Channel do
  use Ecto.Schema
  import Ecto.Changeset

  schema "channels" do
    field :name

    belongs_to :world, ElvenGardUniverse.Env.World
    has_many :maps, ElvenGardUniverse.Env.Map
    has_many :characters, ElvenGardUniverse.Entity.Character

    timestamps()
  end

  @doc false
  def changeset(channel, attrs) do
    channel
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
