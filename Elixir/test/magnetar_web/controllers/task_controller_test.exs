defmodule MagnetarWeb.TaskControllerTest do
  use MagnetarWeb.ConnCase

  alias Magnetar.Todos.Task
  alias Magnetar.Accounts

  @create_attrs %{
    title: "some title",
    description: "some description"
  }
  @update_attrs %{
    title: "some updated title",
    description: "some updated description"
  }
  @invalid_attrs %{title: nil, description: nil}

  setup %{conn: conn} do
    {:ok, user} = Accounts.create_user(%{name: "user", email: "user#{System.unique_integer([:positive])}@example.com"})
    {:ok, conn: put_req_header(conn, "accept", "application/json"), user: user}
  end

  # Helpers
  defp create_task(user) do
    {:ok, task} = Magnetar.Todos.create_task(user.id, @create_attrs)
    task
  end

  describe "index" do
    test "lists all tasks for user", %{conn: conn, user: user} do
      conn = get(conn, Routes.task_path(conn, :index, user.id))
      assert json_response(conn, 200) == []
    end
  end

  describe "create task" do
    test "renders task when data is valid", %{conn: conn, user: user} do
      conn = post(conn, Routes.task_path(conn, :create, user.id), @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)

      conn = get(conn, Routes.task_path(conn, :show, id))

      assert %{
               "id" => ^id,
               "description" => "some description",
               "is_done" => false,
               "title" => "some title",
               "user_id" => _
             } = json_response(conn, 200)
    end

    test "renders errors when data is invalid", %{conn: conn, user: user} do
      conn = post(conn, Routes.task_path(conn, :create, user.id), @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update task" do
    setup %{user: user} do
      task = create_task(user)
      %{task: task}
    end

    test "renders task when data is valid", %{conn: conn, task: %Task{id: id} = task} do
      conn = put(conn, Routes.task_path(conn, :update, task), @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)

      conn = get(conn, Routes.task_path(conn, :show, id))

      assert %{
               "id" => ^id,
               "description" => "some updated description",
               "title" => "some updated title"
             } = json_response(conn, 200)
    end
  end

  describe "done task" do
    setup %{user: user} do
      task = create_task(user)
      %{task: task}
    end

    test "marks task as done", %{conn: conn, task: task} do
      conn = patch(conn, Routes.task_path(conn, :done, task))
      assert %{"is_done" => true} = json_response(conn, 200)
    end
  end

  describe "delete task" do
    setup %{user: user} do
      task = create_task(user)
      %{task: task}
    end

    test "deletes chosen task", %{conn: conn, task: task} do
      conn = delete(conn, Routes.task_path(conn, :delete, task))
      assert response(conn, 204)

      conn = get(conn, Routes.task_path(conn, :show, task))
      assert json_response(conn, 404)["error"] == "Task not found"
    end
  end
end
