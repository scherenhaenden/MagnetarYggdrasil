defmodule MagnetarWeb.ChangesetView do
  use MagnetarWeb, :view

  @doc """
  Traverses and translates changeset errors.

  See `Ecto.Changeset.traverse_errors/2` and
  `MagnetarWeb.ErrorHelpers.translate_error/1` for more details.
  """
  def render("error.json", %{changeset: changeset}) do
    # When encoded, the changeset returns its errors
    # as a JSON object. So we just return it.
    # However, standard practice often formats errors nicely.
    # For now, let's keep it simple.
    %{errors: Ecto.Changeset.traverse_errors(changeset, &translate_error/1)}
  end
end
