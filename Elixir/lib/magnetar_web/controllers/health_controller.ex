defmodule MagnetarWeb.HealthController do
  use MagnetarWeb, :controller

  def index(conn, _params) do
    # Simple health check
    json(conn, %{status: "ok"})
  end
end
