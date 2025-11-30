require 'rails_helper'

RSpec.describe TaskRepository do
  let!(:user) { UserRepository.create(username: "u1", email: "e1") }

  it "creates a task" do
    task = TaskRepository.create(user.id, title: "T1", description: "D1")
    expect(task).to be_persisted
  end
end
