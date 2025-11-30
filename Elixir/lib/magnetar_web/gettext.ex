defmodule MagnetarWeb.Gettext do
  @moduledoc """
  A module providing Internationalization with a gettext-based API.

  By using [Gettext](https://hexdocs.pm/gettext),
  your module is already ready and equipped to handle
  internationalization (i18n).

  Refer to the [Gettext Docs](https://hexdocs.pm/gettext)
  for detailed usage.
  """
  use Gettext.Backend, otp_app: :magnetar
end
