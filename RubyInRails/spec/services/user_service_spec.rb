require 'rails_helper'

RSpec.describe UserService do
  it "creates a user via repository" do
    expect(UserRepository).to receive(:create).with({ username: "u1" })
    UserService.create({ username: "u1" })
  end
end
