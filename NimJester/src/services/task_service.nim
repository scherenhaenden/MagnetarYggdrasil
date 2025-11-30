import std/options
import ../models/task
import ../repositories/task_repo
import ../repositories/user_repo
import user_service # Reuse ServiceError

proc createTaskService*(userId: int, task: TaskCreate): (Option[Task], ServiceError) =
  if task.title == "" or task.description == "":
    return (none(Task), InvalidInput)

  # Check if user exists
  let user = getUserByIdRepo(userId)
  if user.isNone:
    return (none(Task), NotFound) # User not found

  let newTask = createTaskRepo(userId, task)
  return (some(newTask), NoError)

proc getUserTasksService*(userId: int): (seq[Task], ServiceError) =
    # Check if user exists
  let user = getUserByIdRepo(userId)
  if user.isNone:
    return (@[], NotFound) # User not found

  return (getTasksByUserIdRepo(userId), NoError)

proc getTaskService*(id: int): (Option[Task], ServiceError) =
  let task = getTaskByIdRepo(id)
  if task.isNone:
    return (none(Task), NotFound)
  return (task, NoError)

proc updateTaskService*(id: int, title: Option[string], description: Option[string], done: Option[bool]): (Option[Task], ServiceError) =
  let updated = updateTaskRepo(id, title, description, done)
  if updated.isNone:
    return (none(Task), NotFound)
  return (updated, NoError)

proc markTaskDoneService*(id: int): (Option[Task], ServiceError) =
  let updated = markTaskDoneRepo(id)
  if updated.isNone:
    return (none(Task), NotFound)
  return (updated, NoError)

proc deleteTaskService*(id: int): ServiceError =
  if deleteTaskRepo(id):
    return NoError
  return NotFound
