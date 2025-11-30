import Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :magnetar, Magnetar.Repo,
  database: Path.expand("../magnetar_test.db", Path.dirname(__ENV__.file)),
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :magnetar, MagnetarWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "TEST_SECRET_KEY_BASE_TEST_SECRET_KEY_BASE_TEST_SECRET_KEY_BASE",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning
