using Genie.Configuration

const config = Genie.Configuration.Settings(
  server_port                     = 8000,
  server_host                     = "0.0.0.0",
  log_level                       = Genie.Logging.Error,
  log_to_file                     = true,
  server_handle_static_files      = true,
)
