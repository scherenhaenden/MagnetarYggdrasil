require 'rails_helper'

RSpec.describe "Tasks", type: :request do
  let!(:user) { User.create(username: "u1", email: "e1") }

  describe "POST /users/:id/tasks" do
    it "creates a task" do
      post "/users/#{user.id}/tasks", params: { title: "T1", description: "D1" }
      expect(response).to have_http_status(:created)
      expect(JSON.parse(response.body)["title"]).to eq("T1")
      expect(JSON.parse(response.body)["user_id"]).to eq(user.id)
    end
  end

  describe "GET /users/:id/tasks" do
    it "lists tasks" do
      Task.create(user: user, title: "T1", description: "D1")
      get "/users/#{user.id}/tasks"
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body).size).to eq(1)
    end
  end

  describe "GET /tasks/:id" do
    it "gets a task" do
      task = Task.create(user: user, title: "T1", description: "D1")
      get "/tasks/#{task.id}"
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body)["id"]).to eq(task.id)
    end
  end

  describe "PATCH /tasks/:id/done" do
    it "marks task as done" do
      task = Task.create(user: user, title: "T1", description: "D1")
      patch "/tasks/#{task.id}/done"
      expect(response).to have_http_status(:ok)
      expect(JSON.parse(response.body)["done"]).to eq(true)
    end
  end
end
