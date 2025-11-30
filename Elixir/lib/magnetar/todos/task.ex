defmodule Magnetar.Todos.Task do
  use Ecto.Schema
  import Ecto.Changeset

  @derive {Jason.Encoder, only: [:id, :title, :description, :is_done, :user_id]}
  schema "tasks" do
    field :title, :string
    field :description, :string
    field :is_done, :boolean, default: false
    field :user_id, :id

    timestamps()
  end

  @doc false
  def changeset(task, attrs) do
    task
    |> cast(attrs, [:title, :description, :is_done, :user_id])
    |> validate_required([:title, :description, :user_id])
    |> foreign_key_constraint(:user_id)
  end
end
