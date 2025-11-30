class SystemController < ApplicationController
  def health
    render json: { status: "ok", version: "1.0.0" }
  end
end
