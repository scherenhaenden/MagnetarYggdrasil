using Genie.Router
using JuliaGenie.UsersController
using JuliaGenie.TasksController
using Genie.Renderer.Json

route("/") do
  serve_static_file("welcome.html")
end

route("/health") do
  json(Dict("status" => "ok", "version" => "1.0.0"))
end

# Users
route("/users", UsersController.create, method = POST)
route("/users", UsersController.index, method = GET)
route("/users/:id", UsersController.show, method = GET)
route("/users/:id", UsersController.update, method = PUT)
route("/users/:id", UsersController.delete, method = DELETE)

# Tasks
route("/users/:id/tasks", TasksController.create, method = POST)
route("/users/:id/tasks", TasksController.index, method = GET)
route("/tasks/:id", TasksController.show, method = GET)
route("/tasks/:id", TasksController.update, method = PUT)
route("/tasks/:id/done", TasksController.mark_done, method = PATCH)
route("/tasks/:id", TasksController.delete, method = DELETE)
