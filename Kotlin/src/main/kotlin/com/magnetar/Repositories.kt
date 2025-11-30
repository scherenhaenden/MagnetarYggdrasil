package com.magnetar

import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq
import org.jetbrains.exposed.sql.transactions.transaction

class UserRepository {
    fun create(user: CreateUserRequest): User = transaction {
        val id = Users.insert {
            it[name] = user.name
            it[email] = user.email
        } get Users.id

        User(id, user.name, user.email)
    }

    fun findAll(): List<User> = transaction {
        Users.selectAll().map {
            User(
                id = it[Users.id],
                name = it[Users.name],
                email = it[Users.email]
            )
        }
    }

    fun findById(id: Int): User? = transaction {
        Users.select { Users.id eq id }
            .map {
                User(
                    id = it[Users.id],
                    name = it[Users.name],
                    email = it[Users.email]
                )
            }
            .singleOrNull()
    }

    fun update(id: Int, update: UpdateUserRequest): User? = transaction {
        val existing = Users.select { Users.id eq id }.singleOrNull() ?: return@transaction null

        Users.update({ Users.id eq id }) {
            update.name?.let { name -> it[Users.name] = name }
            update.email?.let { email -> it[Users.email] = email }
        }

        User(
            id = id,
            name = update.name ?: existing[Users.name],
            email = update.email ?: existing[Users.email]
        )
    }

    fun delete(id: Int): Boolean = transaction {
        Users.deleteWhere { Users.id eq id } > 0
    }
}

class TaskRepository {
    fun create(userId: Int, task: CreateTaskRequest): Task = transaction {
        val id = Tasks.insert {
            it[Tasks.userId] = userId
            it[title] = task.title
            it[description] = task.description
            it[isDone] = false
        } get Tasks.id

        Task(id, userId, task.title, task.description, false)
    }

    fun findByUserId(userId: Int): List<Task> = transaction {
        Tasks.select { Tasks.userId eq userId }
            .map {
                Task(
                    id = it[Tasks.id],
                    userId = it[Tasks.userId],
                    title = it[Tasks.title],
                    description = it[Tasks.description],
                    isDone = it[Tasks.isDone]
                )
            }
    }

    fun findById(id: Int): Task? = transaction {
        Tasks.select { Tasks.id eq id }
            .map {
                Task(
                    id = it[Tasks.id],
                    userId = it[Tasks.userId],
                    title = it[Tasks.title],
                    description = it[Tasks.description],
                    isDone = it[Tasks.isDone]
                )
            }
            .singleOrNull()
    }

    fun update(id: Int, update: UpdateTaskRequest): Task? = transaction {
        val existing = Tasks.select { Tasks.id eq id }.singleOrNull() ?: return@transaction null

        Tasks.update({ Tasks.id eq id }) {
            update.title?.let { title -> it[Tasks.title] = title }
            update.description?.let { description -> it[Tasks.description] = description }
            update.isDone?.let { isDone -> it[Tasks.isDone] = isDone }
        }

        // Re-fetch to be sure
        val updated = Tasks.select { Tasks.id eq id }.single()

        Task(
            id = id,
            userId = updated[Tasks.userId],
            title = updated[Tasks.title],
            description = updated[Tasks.description],
            isDone = updated[Tasks.isDone]
        )
    }

    fun markDone(id: Int): Task? = transaction {
        val existing = Tasks.select { Tasks.id eq id }.singleOrNull() ?: return@transaction null

        Tasks.update({ Tasks.id eq id }) {
             it[isDone] = true
        }

        Task(
            id = id,
            userId = existing[Tasks.userId],
            title = existing[Tasks.title],
            description = existing[Tasks.description],
            isDone = true
        )
    }

    fun delete(id: Int): Boolean = transaction {
        Tasks.deleteWhere { Tasks.id eq id } > 0
    }
}
