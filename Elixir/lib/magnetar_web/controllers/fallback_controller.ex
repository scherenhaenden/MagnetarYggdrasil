defmodule MagnetarWeb.FallbackController do
  @moduledoc """
  Translates controller return values to valid Plug connection responses.

  For example, `{:error, :not_found}` causes a 404 status,
  and `{:error, changeset}` causes a 422 status.
  """
  use MagnetarWeb, :controller

  # This clause handles errors returned by Ecto's insert/update/delete.
  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(MagnetarWeb.ChangesetView)
    |> render("error.json", changeset: changeset)
  end

  # This clause is an example of how to handle resources that cannot be found.
  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(MagnetarWeb.ErrorView)
    |> render("404.json")
  end
end
