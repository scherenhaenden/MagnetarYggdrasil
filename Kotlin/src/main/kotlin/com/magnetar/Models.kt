package com.magnetar

import kotlinx.serialization.Serializable
import org.jetbrains.exposed.sql.Table

@Serializable
data class User(
    val id: Int,
    val name: String,
    val email: String
)

@Serializable
data class CreateUserRequest(
    val name: String,
    val email: String
)

@Serializable
data class UpdateUserRequest(
    val name: String? = null,
    val email: String? = null
)

@Serializable
data class Task(
    val id: Int,
    val userId: Int,
    val title: String,
    val description: String,
    val isDone: Boolean
)

@Serializable
data class CreateTaskRequest(
    val title: String,
    val description: String
)

@Serializable
data class UpdateTaskRequest(
    val title: String? = null,
    val description: String? = null,
    val isDone: Boolean? = null
)

object Users : Table("users") {
    val id = integer("id").autoIncrement()
    val name = varchar("name", 255)
    val email = varchar("email", 255).uniqueIndex()

    override val primaryKey = PrimaryKey(id)
}

object Tasks : Table("tasks") {
    val id = integer("id").autoIncrement()
    val userId = integer("user_id").references(Users.id)
    val title = varchar("title", 255)
    val description = text("description")
    val isDone = bool("is_done").default(false)

    override val primaryKey = PrimaryKey(id)
}
