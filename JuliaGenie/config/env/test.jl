using Genie.Configuration

const config = Genie.Configuration.Settings(
  server_port                     = 8000,
  server_host                     = "127.0.0.1",
  log_level                       = Genie.Logging.Error,
  log_to_file                     = false,
  server_handle_static_files      = true,
)
