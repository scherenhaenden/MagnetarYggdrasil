package magnetar.routes

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import magnetar.models.{User, Task}
import magnetar.repository.Repository
import spray.json.DefaultJsonProtocol._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

import scala.concurrent.Future

trait JsonSupport {
  implicit val userFormat = jsonFormat5(User.apply)
  implicit val taskFormat = jsonFormat7(Task.apply)
}

class UserRoutes(repository: Repository) extends JsonSupport {

  val routes: Route = pathPrefix("users") {
    concat(
      pathEnd {
        concat(
          get {
            complete(repository.getAllUsers())
          },
          post {
            entity(as[User]) { user =>
              onSuccess(repository.createUser(user)) { createdUser =>
                complete(StatusCodes.Created, createdUser)
              }
            }
          }
        )
      },
      path(LongNumber) { id =>
        concat(
          get {
            onSuccess(repository.getUser(id)) {
              case Some(user) => complete(user)
              case None       => complete(StatusCodes.NotFound)
            }
          },
          put {
            entity(as[User]) { user =>
              onSuccess(repository.updateUser(id, user)) {
                case Some(updatedUser) => complete(updatedUser)
                case None              => complete(StatusCodes.NotFound)
              }
            }
          },
          delete {
            onSuccess(repository.deleteUser(id)) {
              case 0 => complete(StatusCodes.NotFound)
              case _ => complete(StatusCodes.NoContent)
            }
          }
        )
      },
      path(LongNumber / "tasks") { userId =>
        concat(
          get {
             onSuccess(repository.getTasksByUser(userId)) { tasks =>
               complete(tasks)
             }
          },
          post {
            entity(as[Task]) { task =>
              val taskWithUserId = task.copy(userId = userId)
              onSuccess(repository.createTask(taskWithUserId)) { createdTask =>
                 complete(StatusCodes.Created, createdTask)
              }
            }
          }
        )
      }
    )
  }
}

class TaskRoutes(repository: Repository) extends JsonSupport {

  val routes: Route = pathPrefix("tasks") {
    concat(
      path(LongNumber) { id =>
        concat(
          get {
             onSuccess(repository.getTask(id)) {
               case Some(task) => complete(task)
               case None => complete(StatusCodes.NotFound)
             }
          },
          put {
            entity(as[Task]) { task =>
              onSuccess(repository.updateTask(id, task)) {
                case Some(updatedTask) => complete(updatedTask)
                case None => complete(StatusCodes.NotFound)
              }
            }
          },
          delete {
             onSuccess(repository.deleteTask(id)) {
               case 0 => complete(StatusCodes.NotFound)
               case _ => complete(StatusCodes.NoContent)
             }
          }
        )
      },
      path(LongNumber / "done") { id =>
        patch {
           // We expect a boolean via JSON body or query param? Go impl used PATCH /tasks/:tid/done.
           // Usually it might be simpler to just toggle or accept a body.
           // Looking at Go implementation: r.PATCH("/tasks/:tid/done", h.MarkTaskDone)
           // Let's assume it sets it to true, or toggles.
           // Wait, usually PATCH is for partial update.
           // Let's look at the method signature in repository: markTaskDone(id, done)
           // I'll accept a boolean in the body for flexibility.
           entity(as[Boolean]) { done =>
             onSuccess(repository.markTaskDone(id, done)) {
               case Some(updatedTask) => complete(updatedTask)
               case None => complete(StatusCodes.NotFound)
             }
           }
        }
      }
    )
  }
}
