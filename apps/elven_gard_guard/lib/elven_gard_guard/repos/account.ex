defmodule ElvenGardGuard.Account do
  alias ElvenGardGuard
  alias ElvenGardGuard.Postgres
  alias ElvenGardGuard.Account.User

  def authenticate_user(name, password) do
    user = Postgres.get_by!(User, name: name)

    case Argon2.check_pass(user, password) do
      {:ok, _user} = ok ->
        ok

      {:error, "invalid password"} ->
        {:error, :unvalid_credential}
    end
  rescue
    Ecto.NoResultsError ->
      Argon2.no_user_verify()
      {:error, :user_not_found}
  end

  def register_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Postgres.insert()
  end
end
