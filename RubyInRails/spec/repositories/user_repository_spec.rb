require 'rails_helper'

RSpec.describe UserRepository do
  it "creates a user" do
    user = UserRepository.create(username: "test", email: "test@example.com")
    expect(user).to be_persisted
  end

  it "finds all users" do
    UserRepository.create(username: "u1", email: "e1")
    expect(UserRepository.find_all.count).to eq(1)
  end
end
