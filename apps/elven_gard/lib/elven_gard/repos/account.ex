defmodule ElvenGard.AccountRepo do
  alias ElvenGard
  alias ElvenGard.Postgres
  alias ElvenGard.Account.User

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
    {password, attrs_rest} = Map.pop(attrs, :password)

    %User{}
    |> User.changeset(
      Map.put(
        attrs_rest,
        :password_hash,
        Argon2.hash_pwd_salt(password)
      )
    )
    |> Postgres.insert()
  end
end
