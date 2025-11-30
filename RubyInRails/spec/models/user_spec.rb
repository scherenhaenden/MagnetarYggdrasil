require 'rails_helper'

RSpec.describe User, type: :model do
  it "is valid with valid attributes" do
    expect(User.new(username: "test", email: "test@example.com")).to be_valid
  end

  it "is not valid without a username" do
    expect(User.new(username: nil, email: "test@example.com")).to_not be_valid
  end

  it "is not valid without an email" do
    expect(User.new(username: "test", email: nil)).to_not be_valid
  end
end
