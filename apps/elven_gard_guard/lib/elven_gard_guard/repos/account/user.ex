defmodule ElvenGardGuard.Account.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name
    field :password_hash

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :password_hash])
    |> validate_required([:name, :password_hash])
  end
end
