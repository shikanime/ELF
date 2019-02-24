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
    {password, attrs_rest} = Map.pop(attrs, :password)

    %UserSchema{}
    |> UserSchema.changeset(Map.put(
      attrs_rest,
      :password_hash,
      Argon2.hash_pwd_salt(password)
    ))
    |> Postgres.insert()
  end
end
