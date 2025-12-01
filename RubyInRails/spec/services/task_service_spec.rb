require 'rails_helper'

RSpec.describe TaskService do
  let!(:user) { UserRepository.create(username: "u1", email: "e1") }

  it "creates a task via repository" do
    expect(UserRepository).to receive(:find_by_id).with(user.id)
    expect(TaskRepository).to receive(:create).with(user.id, { title: "T1" })
    TaskService.create(user.id, { title: "T1" })
  end
end
