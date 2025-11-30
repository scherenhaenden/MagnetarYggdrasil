package magnetar.repository

import magnetar.models.{User, Task}
import slick.jdbc.SQLiteProfile.api._
import scala.concurrent.{ExecutionContext, Future}
import java.time.Instant

class DatabaseSchema(val db: Database) {

  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def email = column[String]("email", O.Unique)
    def createdAt = column[Option[String]]("created_at")
    def updatedAt = column[Option[String]]("updated_at")

    def * = (id.?, name, email, createdAt, updatedAt).mapTo[User]
  }

  class Tasks(tag: Tag) extends Table[Task](tag, "tasks") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def userId = column[Long]("user_id")
    def title = column[String]("title")
    def description = column[String]("description")
    def done = column[Boolean]("done")
    def createdAt = column[Option[String]]("created_at")
    def updatedAt = column[Option[String]]("updated_at")

    def user = foreignKey("user_fk", userId, users)(_.id, onUpdate = ForeignKeyAction.Cascade, onDelete = ForeignKeyAction.Cascade)

    def * = (id.?, userId, title, description, done, createdAt, updatedAt).mapTo[Task]
  }

  val users = TableQuery[Users]
  val tasks = TableQuery[Tasks]

  def createSchema(): Future[Unit] = {
    db.run((users.schema ++ tasks.schema).createIfNotExists)
  }
}

class Repository(db: Database)(implicit ec: ExecutionContext) {
  val schema = new DatabaseSchema(db)
  import schema._

  // User Operations
  def createUser(user: User): Future[User] = {
    val now = Instant.now().toString
    val userWithDates = user.copy(createdAt = Some(now), updatedAt = Some(now))
    val action = (users returning users.map(_.id)) += userWithDates
    db.run(action).map(id => userWithDates.copy(id = Some(id)))
  }

  def getUser(id: Long): Future[Option[User]] = {
    db.run(users.filter(_.id === id).result.headOption)
  }

  def getAllUsers(): Future[Seq[User]] = {
    db.run(users.result)
  }

  def updateUser(id: Long, user: User): Future[Option[User]] = {
    val now = Instant.now().toString
    val query = users.filter(_.id === id).map(u => (u.name, u.email, u.updatedAt))
    val action = query.update((user.name, user.email, Some(now)))
    db.run(action).flatMap {
      case 0 => Future.successful(None)
      case _ => getUser(id)
    }
  }

  def deleteUser(id: Long): Future[Int] = {
    db.run(users.filter(_.id === id).delete)
  }

  // Task Operations
  def createTask(task: Task): Future[Task] = {
    val now = Instant.now().toString
    val taskWithDates = task.copy(createdAt = Some(now), updatedAt = Some(now))
    val action = (tasks returning tasks.map(_.id)) += taskWithDates
    db.run(action).map(id => taskWithDates.copy(id = Some(id)))
  }

  def getTask(id: Long): Future[Option[Task]] = {
    db.run(tasks.filter(_.id === id).result.headOption)
  }

  def getTasksByUser(userId: Long): Future[Seq[Task]] = {
    db.run(tasks.filter(_.userId === userId).result)
  }

  def updateTask(id: Long, task: Task): Future[Option[Task]] = {
    val now = Instant.now().toString
    val query = tasks.filter(_.id === id).map(t => (t.title, t.description, t.done, t.updatedAt))
    val action = query.update((task.title, task.description, task.done, Some(now)))
    db.run(action).flatMap {
      case 0 => Future.successful(None)
      case _ => getTask(id)
    }
  }

  def markTaskDone(id: Long, done: Boolean): Future[Option[Task]] = {
    val now = Instant.now().toString
    val query = tasks.filter(_.id === id).map(t => (t.done, t.updatedAt))
    val action = query.update((done, Some(now)))
    db.run(action).flatMap {
      case 0 => Future.successful(None)
      case _ => getTask(id)
    }
  }

  def deleteTask(id: Long): Future[Int] = {
    db.run(tasks.filter(_.id === id).delete)
  }
}
