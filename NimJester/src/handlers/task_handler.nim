import jester
import std/json
import std/options
import std/strutils
import ../services/task_service
import ../models/task
import ../services/user_service # for ServiceError enum
import ../utils/response_utils

proc createTask*(request: Request, userId: string): ResponseData =
  let intUserId = try: parseInt(userId) except: -1
  if intUserId == -1: return jsonResponse(Http400, "Invalid User ID")

  var body: TaskCreate
  try:
    body = to(parseJson(request.body), TaskCreate)
  except:
    return jsonResponse(Http400, "Invalid JSON")

  let (task, error) = createTaskService(intUserId, body)
  case error:
  of NoError: return jsonResponse(Http201, task.get)
  of NotFound: return jsonResponse(Http404, "User not found")
  of InvalidInput: return jsonResponse(Http400, "Title and description are required")
  else: return jsonResponse(Http500, "Internal Server Error")

proc getUserTasks*(userId: string): ResponseData =
  let intUserId = try: parseInt(userId) except: -1
  if intUserId == -1: return jsonResponse(Http400, "Invalid User ID")

  let (tasks, error) = getUserTasksService(intUserId)
  case error:
  of NoError: return jsonResponse(Http200, tasks)
  of NotFound: return jsonResponse(Http404, "User not found")
  else: return jsonResponse(Http500, "Internal Server Error")

proc getTask*(id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  let (task, error) = getTaskService(intId)
  case error:
  of NoError: return jsonResponse(Http200, task.get)
  of NotFound: return jsonResponse(Http404, "Task not found")
  else: return jsonResponse(Http500, "Internal Server Error")

proc updateTask*(request: Request, id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  var bodyJson: JsonNode
  try:
    bodyJson = parseJson(request.body)
  except:
    return jsonResponse(Http400, "Invalid JSON")

  var title: Option[string]
  var description: Option[string]
  var done: Option[bool]

  if bodyJson.hasKey("title"): title = some(bodyJson["title"].getStr)
  if bodyJson.hasKey("description"): description = some(bodyJson["description"].getStr)
  if bodyJson.hasKey("done"): done = some(bodyJson["done"].getBool)

  let (task, error) = updateTaskService(intId, title, description, done)
  case error:
  of NoError: return jsonResponse(Http200, task.get)
  of NotFound: return jsonResponse(Http404, "Task not found")
  else: return jsonResponse(Http500, "Internal Server Error")

proc markTaskDone*(id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  let (task, error) = markTaskDoneService(intId)
  case error:
  of NoError: return jsonResponse(Http200, task.get)
  of NotFound: return jsonResponse(Http404, "Task not found")
  else: return jsonResponse(Http500, "Internal Server Error")

proc deleteTask*(id: string): ResponseData =
  let intId = try: parseInt(id) except: -1
  if intId == -1: return jsonResponse(Http400, "Invalid ID")

  let error = deleteTaskService(intId)
  case error:
  of NoError: return jsonResponse(Http204, "")
  of NotFound: return jsonResponse(Http404, "Task not found")
  else: return jsonResponse(Http500, "Internal Server Error")
