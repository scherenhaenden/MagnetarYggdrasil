require "./spec_helper"
require "../src/repositories/user_repository"
require "../src/repositories/task_repository"

describe App::UserRepository do
  it "creates and finds a user" do
    user = App::UserRepository.create("jules", "jules@example.com")
    user.id.should_not be_nil

    found = App::UserRepository.find_by_id(user.id.not_nil!)
    found.should_not be_nil
    found.not_nil!.username.should eq "jules"
  end

  it "updates a user" do
    user = App::UserRepository.create("old", "old@example.com")
    updated = App::UserRepository.update(user.id.not_nil!, "new")
    updated.not_nil!.username.should eq "new"
  end

  it "deletes a user" do
    user = App::UserRepository.create("del", "del@example.com")
    App::UserRepository.delete(user.id.not_nil!).should be_true
    App::UserRepository.find_by_id(user.id.not_nil!).should be_nil
  end
end

describe App::TaskRepository do
  it "creates and finds tasks" do
    user = App::UserRepository.create("tasker", "tasker@example.com")
    task = App::TaskRepository.create(user.id.not_nil!, "My Task", "Do it")

    task.id.should_not be_nil
    task.user_id.should eq user.id

    tasks = App::TaskRepository.find_all_by_user(user.id.not_nil!)
    tasks.size.should eq 1
    tasks.first.title.should eq "My Task"
  end

  it "updates and completes a task" do
    user = App::UserRepository.create("updater", "u@e.com")
    task = App::TaskRepository.create(user.id.not_nil!, "Orig", "Desc")

    updated = App::TaskRepository.update(task.id.not_nil!, "New", "Desc")
    updated.not_nil!.title.should eq "New"

    done_task = App::TaskRepository.mark_done(task.id.not_nil!)
    done_task.not_nil!.done.should be_true
  end
end
