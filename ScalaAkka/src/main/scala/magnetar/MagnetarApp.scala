package magnetar

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import magnetar.repository.Repository
import magnetar.routes.{UserRoutes, TaskRoutes}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object MagnetarApp {
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "MagnetarSystem")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val db = Database.forConfig("magnetar.db")
    val repository = new Repository(db)

    // Create schema if not exists
    repository.schema.createSchema().onComplete {
      case Success(_) => system.log.info("Database schema initialized")
      case Failure(ex) => system.log.error("Failed to initialize database schema", ex)
    }

    val userRoutes = new UserRoutes(repository)
    val taskRoutes = new TaskRoutes(repository)

    val routes = userRoutes.routes ~ taskRoutes.routes ~ path("health") {
      get {
        complete("OK")
      }
    }

    val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)

    bindingFuture.onComplete {
      case Success(binding) =>
        val address = binding.localAddress
        system.log.info(s"Server online at http://${address.getHostString}:${address.getPort}/")
      case Failure(ex) =>
        system.log.error("Failed to bind HTTP endpoint, terminating system", ex)
        system.terminate()
    }
  }
}
