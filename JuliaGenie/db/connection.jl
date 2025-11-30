using SearchLight, SearchLightSQLite

SearchLight.Configuration.load() = Dict{String,Any}(
  "dev" => Dict{String,Any}(
    "adapter" => "SQLite",
    "database" => "db/juliagenie.sqlite"
  ),
  "test" => Dict{String,Any}(
    "adapter" => "SQLite",
    "database" => ":memory:"
  ),
  "prod" => Dict{String,Any}(
    "adapter" => "SQLite",
    "database" => "db/juliagenie.sqlite"
  )
)
