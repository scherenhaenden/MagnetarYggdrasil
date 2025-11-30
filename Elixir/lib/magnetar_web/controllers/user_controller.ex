defmodule MagnetarWeb.UserController do
  use MagnetarWeb, :controller

  alias Magnetar.Accounts
  alias Magnetar.Accounts.User

  action_fallback MagnetarWeb.FallbackController

  def index(conn, _params) do
    users = Accounts.list_users()
    json(conn, users)
  end

  def create(conn, params) do
    # params contain "name" and "email"
    # Ensure we permit only valid keys if needed, but schema handles validation.
    with {:ok, %User{} = user} <- Accounts.create_user(params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.user_path(conn, :show, user))
      |> json(user)
    end
  end

  def show(conn, %{"id" => id}) do
    user = Accounts.get_user(id)
    if user do
      json(conn, user)
    else
      conn
      |> put_status(:not_found)
      |> json(%{error: "User not found"})
    end
  end

  def update(conn, %{"id" => id} = params) do
    user = Accounts.get_user(id)

    if user do
      with {:ok, %User{} = user} <- Accounts.update_user(user, params) do
        json(conn, user)
      end
    else
       conn
       |> put_status(:not_found)
       |> json(%{error: "User not found"})
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Accounts.get_user(id)

    if user do
      with {:ok, %User{}} <- Accounts.delete_user(user) do
        send_resp(conn, :no_content, "")
      end
    else
       conn
       |> put_status(:not_found)
       |> json(%{error: "User not found"})
    end
  end
end
