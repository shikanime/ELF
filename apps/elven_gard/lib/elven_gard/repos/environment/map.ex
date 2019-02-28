defmodule ElvenGard.Environment.Map do
  use Ecto.Schema
  import Ecto.Changeset

  schema "maps" do
    field :name

    belongs_to :world, ElvenGard.Environment.World
    belongs_to :channel, ElvenGard.Environment.Channel

    timestamps()
  end

  @doc false
  def changeset(channel, attrs) do
    channel
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
