require "./spec_helper"

# Kemal has specific helpers for testing handlers if we use `Kemal::Test`.
# But standard Spec with HTTP::Client or just invoking logic is also fine.
# We'll use a mocked approach or integration style if possible.
# Actually, Kemal provides `get`, `post` helpers in `spec/kemal`.
# I need to require "spec-kemal" if I had it, but I don't.
# I will write tests that simulate requests or assumes `kemal` is running?
# No, usually in Crystal you run the app in a separate fiber or use `Kemal.run` with port 0?
# Without `spec-kemal` shard, testing routes is harder.
# But I can use `HTTP::Client` against the running server?
# Or I can just unit test the Controller methods if I refactor them to take context.
# But `App::UserController` defines blocks inside `register`.
# I will write a basic Integration Spec file that would conceptually work.
# Given I cannot add `spec-kemal` easily without internet check, I will assume it's available or write generic HTTP tests.

# Wait, `kemal` shard usually includes testing helpers.
# Let's check `kemal` docs from memory: `require "kemal/spec"` is the standard way.

require "kemal/spec"

describe "SystemController" do
  it "GET /health" do
    get "/health"
    response.status_code.should eq 200
    response.body.should contain "ok"
  end
end

describe "UserController" do
  it "POST /users" do
    headers = HTTP::Headers{"Content-Type" => "application/json"}
    body = {username: "api_user", email: "api@test.com"}.to_json
    post "/users", headers: headers, body: body
    response.status_code.should eq 201
    response.body.should contain "api_user"
  end

  it "GET /users" do
    get "/users"
    response.status_code.should eq 200
  end
end
