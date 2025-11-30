defmodule Magnetar.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  @derive {Jason.Encoder, only: [:id, :name, :email]}
  schema "users" do
    field :name, :string
    field :email, :string
    # field :timestamps, :utc_datetime # handled by timestamps()

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])
    |> validate_required([:name, :email])
    |> validate_format(:email, ~r/@/) # Basic validation
    |> unique_constraint(:email)
  end
end
