import Config

config :magnetar,
  ecto_repos: [Magnetar.Repo],
  generators: [timestamp_type: :utc_datetime]

# Configures the endpoint
config :magnetar, MagnetarWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Phoenix.Endpoint.Cowboy2Adapter,
  render_errors: [
    view: MagnetarWeb.ErrorView,
    accepts: ~w(json),
    layout: false
  ],
  pubsub_server: Magnetar.PubSub,
  live_view: [signing_salt: "SECRET_SALT_CHANGE_ME"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
