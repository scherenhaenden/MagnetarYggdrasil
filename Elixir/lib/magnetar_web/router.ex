defmodule MagnetarWeb.Router do
  use MagnetarWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", MagnetarWeb do
    pipe_through :api

    get "/health", HealthController, :index

    # Users
    get "/users", UserController, :index
    post "/users", UserController, :create
    get "/users/:id", UserController, :show
    put "/users/:id", UserController, :update
    delete "/users/:id", UserController, :delete

    # Tasks
    get "/users/:user_id/tasks", TaskController, :index
    post "/users/:user_id/tasks", TaskController, :create
    get "/tasks/:id", TaskController, :show
    put "/tasks/:id", TaskController, :update
    patch "/tasks/:id/done", TaskController, :done
    delete "/tasks/:id", TaskController, :delete
  end
end
