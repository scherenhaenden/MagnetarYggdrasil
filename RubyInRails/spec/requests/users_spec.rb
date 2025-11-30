require 'rails_helper'

RSpec.describe "Users", type: :request do
  describe "POST /users" do
    it "creates a user" do
      post "/users", params: { username: "jules", email: "jules@example.com" }
      expect(response).to have_http_status(:created)
      expect(JSON.parse(response.body)["username"]).to eq("jules")
    end

    it "returns error on invalid input" do
      post "/users", params: { username: "jules" } # Missing email
      expect(response).to have_http_status(:bad_request)
    end
  end

  describe "GET /users" do
    it "lists users" do
      User.create(username: "u1", email: "e1")
      get "/users"
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body).size).to eq(1)
    end
  end

  describe "GET /users/:id" do
    it "gets a user" do
      user = User.create(username: "u1", email: "e1")
      get "/users/#{user.id}"
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body)["id"]).to eq(user.id)
    end

    it "returns 404 for missing user" do
      get "/users/999"
      expect(response).to have_http_status(:not_found)
    end
  end

  describe "PUT /users/:id" do
    it "updates a user" do
      user = User.create(username: "u1", email: "e1")
      put "/users/#{user.id}", params: { username: "u2" }
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body)["username"]).to eq("u2")
    end
  end

  describe "DELETE /users/:id" do
    it "deletes a user" do
      user = User.create(username: "u1", email: "e1")
      delete "/users/#{user.id}"
      expect(response).to have_http_status(:no_content)
      expect(User.count).to eq(0)
    end
  end
end
