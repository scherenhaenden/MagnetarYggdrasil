package magnetar.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import magnetar.models.{User, Task}
import magnetar.repository.Repository
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.concurrent.ScalaFutures
import slick.jdbc.SQLiteProfile.api._

class TaskRoutesSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with JsonSupport with ScalaFutures {

  val db = Database.forURL("jdbc:sqlite::memory:", driver = "org.sqlite.JDBC")
  val repository = new Repository(db)
  val taskRoutes = new TaskRoutes(repository).routes
  val userRoutes = new UserRoutes(repository).routes

  override def beforeAll(): Unit = {
    repository.schema.createSchema().futureValue
    // Create a user for tasks
    val user = User(None, "Bob", "bob@example.com", None, None)
    Post("/users", user) ~> userRoutes ~> check {
        status should ===(StatusCodes.Created)
    }
  }

  "TaskRoutes" should {
    "create a new task for user" in {
      val task = Task(None, 1, "Task 1", "Description 1", false, None, None)
      Post("/users/1/tasks", task) ~> userRoutes ~> check {
        status should ===(StatusCodes.Created)
        val createdTask = responseAs[Task]
        createdTask.title should ===("Task 1")
        createdTask.userId should ===(1)
      }
    }

    "get all tasks for user" in {
      Get("/users/1/tasks") ~> userRoutes ~> check {
        status should ===(StatusCodes.OK)
        val tasks = responseAs[Seq[Task]]
        tasks should have size 1
        tasks.head.title should ===("Task 1")
      }
    }

    "get task by id" in {
      Get("/tasks/1") ~> taskRoutes ~> check {
        status should ===(StatusCodes.OK)
        responseAs[Task].title should ===("Task 1")
      }
    }

    "update task" in {
      val update = Task(None, 1, "Task 1 Updated", "Description 1", false, None, None)
      Put("/tasks/1", update) ~> taskRoutes ~> check {
        status should ===(StatusCodes.OK)
        responseAs[Task].title should ===("Task 1 Updated")
      }
    }

    "mark task done" in {
      Patch("/tasks/1/done", true) ~> taskRoutes ~> check {
        status should ===(StatusCodes.OK)
        responseAs[Task].done should ===(true)
      }
    }

    "delete task" in {
      Delete("/tasks/1") ~> taskRoutes ~> check {
        status should ===(StatusCodes.NoContent)
      }
      Get("/tasks/1") ~> taskRoutes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }
  }
}
