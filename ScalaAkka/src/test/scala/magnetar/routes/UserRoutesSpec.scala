package magnetar.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.server.Route
import magnetar.models.{User, Task}
import magnetar.repository.Repository
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.concurrent.ScalaFutures
import slick.jdbc.SQLiteProfile.api._
import scala.concurrent.Future

class UserRoutesSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with JsonSupport with ScalaFutures {

  val db = Database.forURL("jdbc:sqlite::memory:", driver = "org.sqlite.JDBC")
  val repository = new Repository(db)
  val userRoutes = new UserRoutes(repository).routes

  override def beforeAll(): Unit = {
    repository.schema.createSchema().futureValue
  }

  "UserRoutes" should {
    "return empty list of users initially" in {
      Get("/users") ~> userRoutes ~> check {
        status should ===(StatusCodes.OK)
        responseAs[Seq[User]] shouldBe empty
      }
    }

    "create a new user" in {
      val user = User(None, "Alice", "alice@example.com", None, None)
      Post("/users", user) ~> userRoutes ~> check {
        status should ===(StatusCodes.Created)
        val createdUser = responseAs[User]
        createdUser.name should ===("Alice")
        createdUser.id shouldBe defined
      }
    }

    "get user by id" in {
        // First create a user (or rely on previous test state if sequential, but better isolate)
        // For simplicity, we rely on the state since db is in-memory and persists for the suite
        // Assuming id 1 from previous test
        Get("/users/1") ~> userRoutes ~> check {
            status should ===(StatusCodes.OK)
            responseAs[User].name should ===("Alice")
        }
    }

    "update user" in {
        val update = User(None, "Alice Updated", "alice@example.com", None, None)
        Put("/users/1", update) ~> userRoutes ~> check {
            status should ===(StatusCodes.OK)
            responseAs[User].name should ===("Alice Updated")
        }
    }

    "delete user" in {
        Delete("/users/1") ~> userRoutes ~> check {
            status should ===(StatusCodes.NoContent)
        }
        Get("/users/1") ~> userRoutes ~> check {
            status should ===(StatusCodes.NotFound)
        }
    }
  }
}
