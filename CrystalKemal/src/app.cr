require "kemal"
require "./db"
require "./controllers/user_controller"
require "./controllers/task_controller"
require "./controllers/system_controller"

# Setup Database
App::Database.setup

# Register Routes
App::UserController.register
App::TaskController.register
App::SystemController.register
