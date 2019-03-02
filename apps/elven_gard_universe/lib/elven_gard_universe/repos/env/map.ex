defmodule ElvenGardUniverse.Env.Map do
  use Ecto.Schema
  import Ecto.Changeset

  schema "maps" do
    field :name

    belongs_to :world, ElvenGardUniverse.Env.World
    belongs_to :channel, ElvenGardUniverse.Env.Channel

    timestamps()
  end

  @doc false
  def changeset(channel, attrs) do
    channel
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
