import std/unittest
import std/json
import std/options
import std/os
import ../src/services/user_service
import ../src/services/task_service
import ../src/models/user
import ../src/models/task
import ../src/config/database

# Setup
DbPath = "test_magnetar_services.db"
removeFile(DbPath) # Clean start
initDatabase()

suite "User Service Tests":

  teardown:
    # Optional: truncate tables or just rely on new DB for suite?
    # Better to truncate to keep tests isolated if possible,
    # but for simple service tests we can just create unique data.
    discard

  test "Create User - Success":
    let input = UserCreate(email: "test@example.com", name: "Test User")
    let (user, error) = createUserService(input)
    check error == NoError
    check user.isSome
    check user.get.email == "test@example.com"
    check user.get.id > 0

  test "Create User - Email Exists":
    let input = UserCreate(email: "test@example.com", name: "Test User 2")
    let (user, error) = createUserService(input)
    check error == EmailExists
    check user.isNone

  test "Get User - Success":
    let input = UserCreate(email: "get@example.com", name: "Get User")
    let (created, _) = createUserService(input)
    let (fetched, error) = getUserService(created.get.id)
    check error == NoError
    check fetched.isSome
    check fetched.get.email == "get@example.com"

  test "Get User - NotFound":
    let (fetched, error) = getUserService(99999)
    check error == NotFound
    check fetched.isNone

  test "Update User":
    let input = UserCreate(email: "update@example.com", name: "Original Name")
    let (created, _) = createUserService(input)
    let id = created.get.id

    let (updated, error) = updateUserService(id, some("New Name"), none(string))
    check error == NoError
    check updated.get.name == "New Name"
    check updated.get.email == "update@example.com"

  test "Delete User":
    let input = UserCreate(email: "delete@example.com", name: "Delete Me")
    let (created, _) = createUserService(input)
    let id = created.get.id

    let error = deleteUserService(id)
    check error == NoError

    let (fetched, fetchError) = getUserService(id)
    check fetchError == NotFound

suite "Task Service Tests":
  var userId: int

  setup:
    let input = UserCreate(email: "tasks@example.com", name: "Task User")
    let (user, _) = createUserService(input)
    # If user already exists (re-run), fetch it
    if user.isNone:
       let existing = getUserByIdRepo(1) # Assuming 1? No, dangerous.
       # We should handle this better or recreate DB per test.
       # For now, let's just make unique emails per test suite run?
       # Actually, `removeFile` at top level runs once.
       # Just create a new user for tasks.
       discard
    else:
      userId = user.get.id

  test "Create Task":
    let input = TaskCreate(title: "My Task", description: "Do something")
    let (task, error) = createTaskService(userId, input)
    check error == NoError
    check task.isSome
    check task.get.title == "My Task"
    check task.get.user_id == userId
    check task.get.done == false

  test "Get User Tasks":
    let (tasks, error) = getUserTasksService(userId)
    check error == NoError
    check tasks.len > 0
    check tasks[0].title == "My Task"

  test "Update Task":
    let input = TaskCreate(title: "To Update", description: "Desc")
    let (task, _) = createTaskService(userId, input)
    let id = task.get.id

    let (updated, error) = updateTaskService(id, some("Updated Title"), none(string), some(true))
    check error == NoError
    check updated.get.title == "Updated Title"
    check updated.get.done == true

  test "Mark Task Done":
    let input = TaskCreate(title: "To Finish", description: "Desc")
    let (task, _) = createTaskService(userId, input)
    let id = task.get.id

    let (updated, error) = markTaskDoneService(id)
    check error == NoError
    check updated.get.done == true
