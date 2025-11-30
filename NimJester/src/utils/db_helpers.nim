import std/db_sqlite

proc execDynamic*(db: DbConn, query: SqlQuery, args: seq[string]) =
  case args.len:
  of 0: db.exec(query)
  of 1: db.exec(query, args[0])
  of 2: db.exec(query, args[0], args[1])
  of 3: db.exec(query, args[0], args[1], args[2])
  of 4: db.exec(query, args[0], args[1], args[2], args[3])
  of 5: db.exec(query, args[0], args[1], args[2], args[3], args[4])
  of 6: db.exec(query, args[0], args[1], args[2], args[3], args[4], args[5])
  else:
    # Fallback or error. For this project, we shouldn't exceed 6 params for updates.
    # User update has max 2 fields + ID = 3 params.
    # Task update has max 3 fields + ID = 4 params.
    raise newException(ValueError, "Too many arguments for execDynamic")
