defmodule ElvenGard.Entity.Hero do
  use Ecto.Schema
  import Ecto.Changeset

  schema "heros" do
    field :name
    field :position_x, :integer
    field :position_y, :integer
    field :gender
    field :hair_style, :integer
    field :hair_color, :integer
    field :authority, :integer
    field :dignity, :integer
    field :compliment, :integer
    field :morph, :integer
    field :invisible, :boolean
    field :sp_upgrade, :integer
    field :arena_winner, :integer

    belongs_to :user, ElvenGard.Account.User
    belongs_to :class, ElvenGard.Society.Class
    belongs_to :group, ElvenGard.Society.Group
    belongs_to :channel, ElvenGard.Environment.Channel
    has_one :family, ElvenGard.Entity.Family

    timestamps()
  end

  @doc false
  def changeset(hero, attrs) do
    hero
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
