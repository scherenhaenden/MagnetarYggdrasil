require 'rails_helper'

RSpec.describe Task, type: :model do
  let(:user) { User.create(username: "test", email: "test@example.com") }

  it "is valid with valid attributes" do
    expect(Task.new(user: user, title: "Task 1", description: "Desc")).to be_valid
  end

  it "is not valid without a title" do
    expect(Task.new(user: user, title: nil, description: "Desc")).to_not be_valid
  end
end
