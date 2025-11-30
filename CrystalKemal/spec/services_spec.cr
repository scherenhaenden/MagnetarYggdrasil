require "./spec_helper"

# Since Service layer just delegates to Repository in this simple app,
# testing Repositories covers the logic.
# However, for 100% coverage and adherence to architecture, we should have them.

describe App::UserService do
  it "creates user" do
    user = App::UserService.create_user("service_user", "s@e.com")
    user.username.should eq "service_user"
  end
end

describe App::TaskService do
  it "creates task" do
    user = App::UserService.create_user("task_service", "t@e.com")
    task = App::TaskService.create_task(user.id.not_nil!, "Service Task", "D")
    task.title.should eq "Service Task"
  end
end
