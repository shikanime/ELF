defmodule ElvenGardAuth.AccountRepo do
  @moduledoc """
  The Accounts context.
  """

  alias ElvenGardAuth.Repo
  alias ElvenGardAuth.UserSchema

  @doc """
  Returns the list of UserSchemas.

  ## Examples

      iex> list_user()
      [%UserSchema{}, ...]

  """
  def list_user do
    Repo.all(UserSchema)
  end

  @doc """
  Gets a single UserSchema.

  Raises `Ecto.NoResultsError` if the UserSchema does not exist.

  ## Examples

      iex> get_user!(123)
      %UserSchema{}

      iex> get_user!(456)
      ** (Ecto.NoResultsError)

  """
  def get_user!(id), do: Repo.get!(UserSchema, id)

  @doc """
  Creates a UserSchema.

  ## Examples

      iex> create_user(%{field: value})
      {:ok, %UserSchema{}}

      iex> create_user(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_user(attrs \\ %{}) do
    %UserSchema{}
    |> UserSchema.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a UserSchema.

  ## Examples

      iex> update_user(UserSchema, %{field: new_value})
      {:ok, %UserSchema{}}

      iex> update_user(UserSchema, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_user(%UserSchema{} = UserSchema, attrs) do
    UserSchema
    |> UserSchema.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a UserSchema.

  ## Examples

      iex> delete_user(UserSchema)
      {:ok, %UserSchema{}}

      iex> delete_user(UserSchema)
      {:error, %Ecto.Changeset{}}

  """
  def delete_user(%UserSchema{} = UserSchema) do
    Repo.delete(UserSchema)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking UserSchema changes.

  ## Examples

      iex> change_user(UserSchema)
      %Ecto.Changeset{source: %UserSchema{}}

  """
  def change_user(%UserSchema{} = UserSchema) do
    UserSchema.changeset(UserSchema, %{})
  end
end
