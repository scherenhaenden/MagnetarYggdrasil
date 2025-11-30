require 'rails_helper'

RSpec.describe "System", type: :request do
  describe "GET /health" do
    it "returns health status" do
      get "/health"
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body)).to eq({ "status" => "ok", "version" => "1.0.0" })
    end
  end
end
