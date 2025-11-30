package magnetar.models

final case class User(
  id: Option[Long],
  name: String,
  email: String,
  createdAt: Option[String],
  updatedAt: Option[String]
)

final case class Task(
  id: Option[Long],
  userId: Long,
  title: String,
  description: String,
  done: Boolean,
  createdAt: Option[String],
  updatedAt: Option[String]
)

final case class UserTasks(
  user: User,
  tasks: Seq[Task]
)
