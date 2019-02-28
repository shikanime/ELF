defmodule ElvenGard.Environment.Channel do
  use Ecto.Schema
  import Ecto.Changeset

  schema "channels" do
    field :name

    belongs_to :world, ElvenGard.Environment.World
    has_many :maps, ElvenGard.Environment.Map
    has_many :users, ElvenGard.Entity.Hero

    timestamps()
  end

  @doc false
  def changeset(channel, attrs) do
    channel
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
