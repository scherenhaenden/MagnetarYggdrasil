defmodule MagnetarWeb.TaskController do
  use MagnetarWeb, :controller

  alias Magnetar.Todos
  alias Magnetar.Todos.Task

  action_fallback MagnetarWeb.FallbackController

  def index(conn, %{"user_id" => user_id}) do
    tasks = Todos.list_tasks_for_user(user_id)
    json(conn, tasks)
  end

  def create(conn, %{"user_id" => user_id} = params) do
    with {:ok, %Task{} = task} <- Todos.create_task(user_id, params) do
      conn
      |> put_status(:created)
      # |> put_resp_header("location", Routes.task_path(conn, :show, task)) # If we had a direct show path
      |> json(task)
    end
  end

  def show(conn, %{"id" => id}) do
    task = Todos.get_task(id)
    if task do
      json(conn, task)
    else
      conn
      |> put_status(:not_found)
      |> json(%{error: "Task not found"})
    end
  end

  def update(conn, %{"id" => id} = params) do
    task = Todos.get_task(id)
    if task do
      with {:ok, %Task{} = task} <- Todos.update_task(task, params) do
        json(conn, task)
      end
    else
      conn
      |> put_status(:not_found)
      |> json(%{error: "Task not found"})
    end
  end

  def done(conn, %{"id" => id}) do
    task = Todos.get_task(id)
    if task do
      with {:ok, %Task{} = task} <- Todos.mark_task_done(task) do
        json(conn, task)
      end
    else
      conn
      |> put_status(:not_found)
      |> json(%{error: "Task not found"})
    end
  end

  def delete(conn, %{"id" => id}) do
    task = Todos.get_task(id)
    if task do
      with {:ok, %Task{}} <- Todos.delete_task(task) do
        send_resp(conn, :no_content, "")
      end
    else
      conn
      |> put_status(:not_found)
      |> json(%{error: "Task not found"})
    end
  end
end
