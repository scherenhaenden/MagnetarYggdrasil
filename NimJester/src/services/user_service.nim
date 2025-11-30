import std/options
import ../models/user
import ../repositories/user_repo

type
  ServiceError* = enum
    NoError
    NotFound
    EmailExists
    InvalidInput

proc createUserService*(user: UserCreate): (Option[User], ServiceError) =
  if user.email == "" or user.name == "":
    return (none(User), InvalidInput)

  # Check if email exists
  let existing = getUserByEmailRepo(user.email)
  if existing.isSome:
    return (none(User), EmailExists)

  let newUser = createUserRepo(user)
  return (some(newUser), NoError)

proc getUserService*(id: int): (Option[User], ServiceError) =
  let user = getUserByIdRepo(id)
  if user.isNone:
    return (none(User), NotFound)
  return (user, NoError)

proc getAllUsersService*(): seq[User] =
  return getAllUsersRepo()

proc updateUserService*(id: int, name: Option[string], email: Option[string]): (Option[User], ServiceError) =
  if email.isSome:
    # Check if new email is taken by another user
    let existing = getUserByEmailRepo(email.get)
    if existing.isSome and existing.get.id != id:
      return (none(User), EmailExists)

  let updated = updateUserRepo(id, name, email)
  if updated.isNone:
    return (none(User), NotFound)
  return (updated, NoError)

proc deleteUserService*(id: int): ServiceError =
  if deleteUserRepo(id):
    return NoError
  return NotFound
