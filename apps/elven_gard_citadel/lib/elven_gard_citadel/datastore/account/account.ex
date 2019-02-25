defmodule ElvenGardCitadel.Datastore.Account do
  @moduledoc """
  The Accounts context.
  """

  require Logger

  alias ElvenGardCitadel.Datastore
  alias ElvenGardCitadel.Datastore.Account.User

  def identify_user(name, password) do
    case Datastore.get_by!(User, name: name)
         |> Argon2.check_pass(password) do
      {:ok, user} ->
        {:ok, user}

      {:error, "invalid password"} ->
        {:error, :unmatch_credential}
    end
  rescue
    e in Ecto.NoResultsError ->
      Logger.warn(fn ->
        e.message
      end)

      Argon2.no_user_verify()

      {:error, :user_not_found}
  end

  def register_user(attrs \\ %{}) do
    {password, attrs_rest} = Map.pop(attrs, :password)

    %User{}
    |> User.changeset(
      Map.put(
        attrs_rest,
        :password_hash,
        Argon2.hash_pwd_salt(password)
      )
    )
    |> Datastore.insert()
  end
end
