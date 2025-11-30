import jester
import std/json
import std/options
import std/strutils
import ../services/user_service
import ../models/user
import ../utils/response_utils

proc createUser*(request: Request): ResponseData =
  var body: UserCreate
  try:
    body = to(parseJson(request.body), UserCreate)
  except:
    return jsonResponse(Http400, "Invalid JSON")

  let (user, error) = createUserService(body)
  case error:
  of NoError: return jsonResponse(Http201, user.get)
  of EmailExists: return jsonResponse(Http409, "Email already exists")
  of InvalidInput: return jsonResponse(Http400, "Name and email are required")
  else: return jsonResponse(Http500, "Internal Server Error")

proc getUsers*(): ResponseData =
  let users = getAllUsersService()
  return jsonResponse(Http200, users)

proc getUser*(id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  let (user, error) = getUserService(intId)
  case error:
  of NoError: return jsonResponse(Http200, user.get)
  of NotFound: return jsonResponse(Http404, "User not found")
  else: return jsonResponse(Http500, "Internal Server Error")

proc updateUser*(request: Request, id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  var bodyJson: JsonNode
  try:
    bodyJson = parseJson(request.body)
  except:
    return jsonResponse(Http400, "Invalid JSON")

  var name: Option[string]
  var email: Option[string]

  if bodyJson.hasKey("name"): name = some(bodyJson["name"].getStr)
  if bodyJson.hasKey("email"): email = some(bodyJson["email"].getStr)

  let (user, error) = updateUserService(intId, name, email)
  case error:
  of NoError: return jsonResponse(Http200, user.get)
  of NotFound: return jsonResponse(Http404, "User not found")
  of EmailExists: return jsonResponse(Http409, "Email already exists")
  else: return jsonResponse(Http500, "Internal Server Error")

proc deleteUser*(id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  let error = deleteUserService(intId)
  case error:
  of NoError: return jsonResponse(Http204, "") # 204 No Content
  of NotFound: return jsonResponse(Http404, "User not found")
  else: return jsonResponse(Http500, "Internal Server Error")
