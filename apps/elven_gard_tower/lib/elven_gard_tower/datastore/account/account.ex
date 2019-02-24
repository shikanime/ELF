defmodule ElvenGardTower.Datastore.Account do
  @moduledoc """
  The Accounts context.
  """

  require Logger

  alias ElvenGardTower.Datastore
  alias ElvenGardTower.Datastore.Account.User

  def identify_user(name, password) do
    Datastore.get_by!(User, name: name)
    |> Argon2.check_pass(password)
  rescue
    e in Ecto.NoResultsError ->
      Logger.warn(fn ->
        e.message
      end)

      {:error, :user_not_found}
  end

  def register_user(attrs \\ %{}) do
    {password, attrs_rest} = Map.pop(attrs, :password)

    %User{}
    |> User.changeset(Map.put(
      attrs_rest,
      :password_hash,
      Argon2.hash_pwd_salt(password)
    ))
    |> Datastore.insert()
  end
end
