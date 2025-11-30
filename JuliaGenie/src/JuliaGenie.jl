module JuliaGenie

using Genie, Logging
using SearchLight, SearchLightSQLite

# Load DB configuration
include("../db/connection.jl")

# Include the MVC layers
# Models
include("models/User.jl")
include("models/Task.jl")

# Repositories
include("repositories/UsersRepository.jl")
include("repositories/TasksRepository.jl")

# Services
include("services/UsersService.jl")
include("services/TasksService.jl")

# Controllers
include("controllers/UsersController.jl")
include("controllers/TasksController.jl")

function connect_db()
  # Configure SearchLight
  SearchLight.Configuration.load() |> SearchLight.connect

  # Ensure migrations are run
  try
    # SearchLight expects migrations in db/migrations relative to the project root
    SearchLight.Migration.init()
    SearchLight.Migration.up()
  catch e
    @warn "Migration status: $e"
  end

  # Enable Foreign Keys
  SearchLight.query("PRAGMA foreign_keys = ON")
end

function main()
  # Database setup
  connect_db()

  Genie.genie(; context = @__MODULE__)
end

end
