defmodule Magnetar.Repo do
  use Ecto.Repo,
    otp_app: :magnetar,
    adapter: Ecto.Adapters.SQLite3
end
