import jester
import handlers/user_handler
import handlers/task_handler
import handlers/health_handler
import config/database
import utils/response_utils

# Initialize database
initDatabase()

template sendResp(responseData: ResponseData) =
  resp responseData.code, responseData.body, "application/json"

routes:
  # Health
  get "/health":
    sendResp healthCheck()

  # Users
  post "/users":
    sendResp createUser(request)
  get "/users":
    sendResp getUsers()
  get "/users/@id":
    sendResp getUser(@"id")
  put "/users/@id":
    sendResp updateUser(request, @"id")
  delete "/users/@id":
    sendResp deleteUser(@"id")

  # Tasks
  post "/users/@id/tasks":
    sendResp createTask(request, @"id")
  get "/users/@id/tasks":
    sendResp getUserTasks(@"id")
  get "/tasks/@id":
    sendResp getTask(@"id")
  put "/tasks/@id":
    sendResp updateTask(request, @"id")
  patch "/tasks/@id/done":
    sendResp markTaskDone(@"id")
  delete "/tasks/@id":
    sendResp deleteTask(@"id")
