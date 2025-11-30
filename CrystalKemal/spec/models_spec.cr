require "./spec_helper"
require "../src/models/user"
require "../src/models/task"

describe App::User do
  it "initializes correctly" do
    user = App::User.new("testuser", "test@example.com")
    user.username.should eq "testuser"
    user.email.should eq "test@example.com"
    user.id.should be_nil
  end

  it "serializes to JSON" do
    user = App::User.new("testuser", "test@example.com", 1)
    json = user.to_json
    json.should contain "testuser"
    json.should contain "test@example.com"
    json.should contain "1"
  end
end

describe App::Task do
  it "initializes correctly" do
    task = App::Task.new(1, "Title", "Desc")
    task.user_id.should eq 1
    task.title.should eq "Title"
    task.description.should eq "Desc"
    task.done.should be_false
  end
end
