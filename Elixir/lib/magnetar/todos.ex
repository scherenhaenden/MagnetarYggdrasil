defmodule Magnetar.Todos do
  @moduledoc """
  The Todos context (Service Layer for Tasks).
  """

  import Ecto.Query, warn: false
  alias Magnetar.Repo
  alias Magnetar.Todos.Task

  def list_tasks_for_user(user_id) do
    # Verify user exists first? The requirement implies CRUD for tasks.
    # Typically GET /users/{id}/tasks
    query = from t in Task, where: t.user_id == ^user_id
    Repo.all(query)
  end

  def get_task!(id), do: Repo.get!(Task, id)
  def get_task(id), do: Repo.get(Task, id)

  def create_task(user_id, attrs \\ %{}) do
    attrs = Map.put(attrs, "user_id", user_id)
    %Task{}
    |> Task.changeset(attrs)
    |> Repo.insert()
  end

  def update_task(%Task{} = task, attrs) do
    task
    |> Task.changeset(attrs)
    |> Repo.update()
  end

  def mark_task_done(%Task{} = task) do
    task
    |> Task.changeset(%{is_done: true})
    |> Repo.update()
  end

  def delete_task(%Task{} = task) do
    Repo.delete(task)
  end

  def change_task(%Task{} = task, attrs \\ %{}) do
    Task.changeset(task, attrs)
  end
end
