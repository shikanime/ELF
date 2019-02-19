defmodule ElvenGardTower.AccountRepo do
  @moduledoc """
  The Accounts context.
  """

  alias ElvenGardTower.Postgres
  alias ElvenGardTower.UserSchema

  def identify_user(name, password) do
    Postgres.get_by!(UserSchema, name: name)
    |> Argon2.check_pass(password)
  end

  def register_user(attrs \\ %{}) do
    %UserSchema{}
    |> UserSchema.changeset(attrs)
    |> Postgres.insert()
  end
end
