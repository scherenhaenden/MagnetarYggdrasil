import std/db_sqlite
import std/strutils
import std/options
import ../config/database
import ../models/user
import ../utils/db_helpers

proc createUserRepo*(user: UserCreate): User =
  let db = getDb()
  defer: db.close()

  let id = db.insertID(sql"INSERT INTO users (email, name) VALUES (?, ?)", user.email, user.name)
  result = User(id: id.int, email: user.email, name: user.name)

proc getUserByIdRepo*(id: int): Option[User] =
  let db = getDb()
  defer: db.close()

  let row = db.getRow(sql"SELECT id, email, name FROM users WHERE id = ?", id)
  if row[0] == "":
    return none(User)

  result = some(User(
    id: parseInt(row[0]),
    email: row[1],
    name: row[2]
  ))

proc getUserByEmailRepo*(email: string): Option[User] =
  let db = getDb()
  defer: db.close()

  let row = db.getRow(sql"SELECT id, email, name FROM users WHERE email = ?", email)
  if row[0] == "":
    return none(User)

  result = some(User(
    id: parseInt(row[0]),
    email: row[1],
    name: row[2]
  ))

proc getAllUsersRepo*(): seq[User] =
  let db = getDb()
  defer: db.close()

  result = @[]
  for row in db.rows(sql"SELECT id, email, name FROM users"):
    result.add(User(
      id: parseInt(row[0]),
      email: row[1],
      name: row[2]
    ))

proc updateUserRepo*(id: int, name: Option[string], email: Option[string]): Option[User] =
  let db = getDb()
  defer: db.close()

  # First check if user exists
  let existing = getUserByIdRepo(id)
  if existing.isNone:
    return none(User)

  var query = "UPDATE users SET "
  var args: seq[string] = @[]
  var updates: seq[string] = @[]

  if name.isSome:
    updates.add("name = ?")
    args.add(name.get)

  if email.isSome:
    updates.add("email = ?")
    args.add(email.get)

  if updates.len == 0:
    return existing

  query &= updates.join(", ")
  query &= " WHERE id = ?"
  args.add($id)

  db.execDynamic(sql(query), args)
  return getUserByIdRepo(id)

proc deleteUserRepo*(id: int): bool =
  let db = getDb()
  defer: db.close()

  let affected = db.execAffectedRows(sql"DELETE FROM users WHERE id = ?", id)
  return affected > 0
