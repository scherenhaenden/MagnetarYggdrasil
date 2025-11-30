import Config

# For production, don't forget to configure the url host
# to something meaningful, Phoenix uses this information
# when generating URLs.

config :magnetar, MagnetarWeb.Endpoint,
  url: [host: "example.com", port: 80],
  cache_static_manifest: "priv/static/cache_manifest.json"

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :magnetar, Magnetar.Mailer, adapter: Swoosh.Adapters.Local

# Do not print debug messages in production
config :logger, level: :info

# Runtime production configuration, including reading
# of environment variables, is done on config/runtime.exs.
