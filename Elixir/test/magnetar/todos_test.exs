defmodule Magnetar.TodosTest do
  use Magnetar.DataCase

  alias Magnetar.Todos

  describe "tasks" do
    alias Magnetar.Todos.Task
    alias Magnetar.Accounts

    @invalid_attrs %{title: nil, description: nil}

    setup do
      {:ok, user} = Accounts.create_user(%{name: "user", email: "user@example.com"})
      %{user: user}
    end

    test "list_tasks_for_user/1 returns tasks for user", %{user: user} do
      {:ok, task} = Todos.create_task(user.id, %{title: "task", description: "desc"})
      assert Todos.list_tasks_for_user(user.id) == [task]
    end

    test "get_task!/1 returns the task with given id", %{user: user} do
      {:ok, task} = Todos.create_task(user.id, %{title: "task", description: "desc"})
      assert Todos.get_task!(task.id) == task
    end

    test "create_task/2 with valid data creates a task", %{user: user} do
      valid_attrs = %{title: "some title", description: "some description"}

      assert {:ok, %Task{} = task} = Todos.create_task(user.id, valid_attrs)
      assert task.title == "some title"
      assert task.description == "some description"
      assert task.user_id == user.id
    end

    test "create_task/2 with invalid data returns error changeset", %{user: user} do
      assert {:error, %Ecto.Changeset{}} = Todos.create_task(user.id, @invalid_attrs)
    end

    test "update_task/2 with valid data updates the task", %{user: user} do
      {:ok, task} = Todos.create_task(user.id, %{title: "task", description: "desc"})
      update_attrs = %{title: "some updated title"}

      assert {:ok, %Task{} = task} = Todos.update_task(task, update_attrs)
      assert task.title == "some updated title"
    end

    test "mark_task_done/1 marks task as done", %{user: user} do
      {:ok, task} = Todos.create_task(user.id, %{title: "task", description: "desc"})
      assert task.is_done == false
      assert {:ok, %Task{} = task} = Todos.mark_task_done(task)
      assert task.is_done == true
    end

    test "delete_task/1 deletes the task", %{user: user} do
      {:ok, task} = Todos.create_task(user.id, %{title: "task", description: "desc"})
      assert {:ok, %Task{}} = Todos.delete_task(task)
      assert_raise Ecto.NoResultsError, fn -> Todos.get_task!(task.id) end
    end
  end
end
