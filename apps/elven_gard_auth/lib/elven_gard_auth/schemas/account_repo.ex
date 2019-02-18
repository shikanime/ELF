defmodule ElvenGardAuth.AccountRepo do
  @moduledoc """
  The Accounts context.
  """

  alias ElvenGardAuth.Repo
  alias ElvenGardAuth.UserSchema

  @doc """
  Returns the list of UserSchemas.

  ## Examples

      iex> list_UserSchemas()
      [%UserSchema{}, ...]

  """
  def list_UserSchemas do
    Repo.all(UserSchema)
  end

  @doc """
  Gets a single UserSchema.

  Raises `Ecto.NoResultsError` if the UserSchema does not exist.

  ## Examples

      iex> get_UserSchema!(123)
      %UserSchema{}

      iex> get_UserSchema!(456)
      ** (Ecto.NoResultsError)

  """
  def get_UserSchema!(id), do: Repo.get!(UserSchema, id)

  @doc """
  Creates a UserSchema.

  ## Examples

      iex> create_UserSchema(%{field: value})
      {:ok, %UserSchema{}}

      iex> create_UserSchema(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_UserSchema(attrs \\ %{}) do
    %UserSchema{}
    |> UserSchema.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a UserSchema.

  ## Examples

      iex> update_UserSchema(UserSchema, %{field: new_value})
      {:ok, %UserSchema{}}

      iex> update_UserSchema(UserSchema, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_UserSchema(%UserSchema{} = UserSchema, attrs) do
    UserSchema
    |> UserSchema.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a UserSchema.

  ## Examples

      iex> delete_UserSchema(UserSchema)
      {:ok, %UserSchema{}}

      iex> delete_UserSchema(UserSchema)
      {:error, %Ecto.Changeset{}}

  """
  def delete_UserSchema(%UserSchema{} = UserSchema) do
    Repo.delete(UserSchema)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking UserSchema changes.

  ## Examples

      iex> change_UserSchema(UserSchema)
      %Ecto.Changeset{source: %UserSchema{}}

  """
  def change_UserSchema(%UserSchema{} = UserSchema) do
    UserSchema.changeset(UserSchema, %{})
  end
end
