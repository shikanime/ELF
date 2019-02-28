defmodule ElvenGard.Account.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name
    field :password_hash

    has_many :heros, ElvenGard.Entity.Hero

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :password_hash])
    |> validate_required([:name, :password_hash])
  end
end
