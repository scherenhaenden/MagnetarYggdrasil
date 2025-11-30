import std/db_sqlite
import std/strutils
import std/options
import ../config/database
import ../models/task
import ../utils/db_helpers

proc createTaskRepo*(userId: int, task: TaskCreate): Task =
  let db = getDb()
  defer: db.close()

  let id = db.insertID(sql"INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0)", userId, task.title, task.description)
  result = Task(id: id.int, user_id: userId, title: task.title, description: task.description, done: false)

proc getTaskByIdRepo*(id: int): Option[Task] =
  let db = getDb()
  defer: db.close()

  let row = db.getRow(sql"SELECT id, user_id, title, description, done FROM tasks WHERE id = ?", id)
  if row[0] == "":
    return none(Task)

  result = some(Task(
    id: parseInt(row[0]),
    user_id: parseInt(row[1]),
    title: row[2],
    description: row[3],
    done: parseBool(row[4]) # SQLite stores boolean as 0/1, Nim's db_sqlite might return "0"/"1" or "f"/"t"? Let's verify.
    # Actually, SQLite standard is 0/1. `parseBool` in Nim parses "y", "yes", "true", "1", "on" as true. So "1" works.
  ))

proc getTasksByUserIdRepo*(userId: int): seq[Task] =
  let db = getDb()
  defer: db.close()

  result = @[]
  for row in db.rows(sql"SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?", userId):
    result.add(Task(
      id: parseInt(row[0]),
      user_id: parseInt(row[1]),
      title: row[2],
      description: row[3],
      done: parseBool(row[4])
    ))

proc updateTaskRepo*(id: int, title: Option[string], description: Option[string], done: Option[bool]): Option[Task] =
  let db = getDb()
  defer: db.close()

  # First check if task exists
  let existing = getTaskByIdRepo(id)
  if existing.isNone:
    return none(Task)

  var query = "UPDATE tasks SET "
  var args: seq[string] = @[]
  var updates: seq[string] = @[]

  if title.isSome:
    updates.add("title = ?")
    args.add(title.get)

  if description.isSome:
    updates.add("description = ?")
    args.add(description.get)

  if done.isSome:
    updates.add("done = ?")
    args.add(if done.get: "1" else: "0")

  if updates.len == 0:
    return existing

  query &= updates.join(", ")
  query &= " WHERE id = ?"
  args.add($id)

  db.execDynamic(sql(query), args)
  return getTaskByIdRepo(id)

proc markTaskDoneRepo*(id: int): Option[Task] =
  let db = getDb()
  defer: db.close()

  let existing = getTaskByIdRepo(id)
  if existing.isNone:
    return none(Task)

  db.exec(sql"UPDATE tasks SET done = 1 WHERE id = ?", id)
  return getTaskByIdRepo(id)

proc deleteTaskRepo*(id: int): bool =
  let db = getDb()
  defer: db.close()

  let affected = db.execAffectedRows(sql"DELETE FROM tasks WHERE id = ?", id)
  return affected > 0
