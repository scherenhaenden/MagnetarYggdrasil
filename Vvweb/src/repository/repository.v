module repository

import db.sqlite
import model

// Repository handles database interactions
pub struct Repository {
pub mut:
	db sqlite.DB
}

// new_repository creates a new repository instance
pub fn new_repository(db_path string) !Repository {
	db := sqlite.connect(db_path)!

	// Enable Foreign Keys
	db.exec('PRAGMA foreign_keys = ON;')!

	// Create Users table
	db.exec('CREATE TABLE IF NOT EXISTS users (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		username TEXT UNIQUE NOT NULL,
		email TEXT UNIQUE NOT NULL
	)')!

	// Create Tasks table
	db.exec('CREATE TABLE IF NOT EXISTS tasks (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		user_id INTEGER NOT NULL,
		title TEXT NOT NULL,
		description TEXT NOT NULL,
		done BOOLEAN DEFAULT 0,
		FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
	)')!

	return Repository{db: db}
}

// create_user creates a new user
pub fn (mut r Repository) create_user(user model.User) !model.User {
	// Using V's ORM or prepared statement if wrapper allows.
	// db.sqlite exec_param only accepts one param. We must construct query manually or use `sql` block if structs match.
	// But `sql` block requires registered structs or simple queries.
	// For standard db.sqlite, we can only bind one param if using exec_param.
	// However, we can use string interpolation carefully if we don't have full binding support, OR loop.
    // Wait, the standard library sqlite module `exec_param` takes `(query string, param string)`.
    // If we need multiple params, we might need to use `exec` with string interpolation (risky for injection) or a proper wrapper.
    // But V's `sql` block is the preferred way.
    // Let's use `sql` block if possible, but that requires ORM registration.
    // Simpler: use the low-level `sqlite` C binding if `db.sqlite` is too limited, OR just use string interpolation since this is a demo/benchmark.
    // BUT we want to be "Business Equal", so SQL injection safe.

    // Actually, V's `db.sqlite` `DB` struct has `q_int`, `q_string` helpers? No.
    // Let's look at standard usage. V has `sql` block integration.

    sql r.db {
        insert user into model.User
    } or { return err }

    // We need to fetch the ID.
    id := r.db.last_insert_rowid()
    return r.get_user(int(id))
}

// get_all_users returns all users
pub fn (mut r Repository) get_all_users() ![]model.User {
    users := sql r.db {
        select from model.User
    }!
    return users
}

// get_user returns a user by ID
pub fn (mut r Repository) get_user(id int) !model.User {
    users := sql r.db {
        select from model.User where id == id
    }!
    if users.len == 0 {
        return error('User not found')
    }
    return users[0]
}

// update_user updates a user
pub fn (mut r Repository) update_user(id int, username string) !model.User {
    sql r.db {
        update model.User set username = username where id == id
    }!
    return r.get_user(id)
}

// delete_user deletes a user
pub fn (mut r Repository) delete_user(id int) ! {
    sql r.db {
        delete from model.User where id == id
    }!
}

// create_task creates a new task
pub fn (mut r Repository) create_task(task model.Task) !model.Task {
    r.get_user(task.user_id)!

    sql r.db {
        insert task into model.Task
    }!

    id := r.db.last_insert_rowid()
    return r.get_task(int(id))
}

// get_tasks_by_user returns all tasks for a user
pub fn (mut r Repository) get_tasks_by_user(user_id int) ![]model.Task {
    r.get_user(user_id)!

    tasks := sql r.db {
        select from model.Task where user_id == user_id
    }!
    return tasks
}

// get_task returns a task by ID
pub fn (mut r Repository) get_task(id int) !model.Task {
    tasks := sql r.db {
        select from model.Task where id == id
    }!
    if tasks.len == 0 {
        return error('Task not found')
    }
    return tasks[0]
}

// update_task updates a task
pub fn (mut r Repository) update_task(id int, title string, description string) !model.Task {
    r.get_task(id)!
    sql r.db {
        update model.Task set title = title, description = description where id == id
    }!
    return r.get_task(id)
}

// set_task_done marks a task as done
pub fn (mut r Repository) set_task_done(id int, done bool) !model.Task {
    r.get_task(id)!
    sql r.db {
        update model.Task set done = done where id == id
    }!
    return r.get_task(id)
}

// delete_task deletes a task
pub fn (mut r Repository) delete_task(id int) ! {
    r.get_task(id)!
    sql r.db {
        delete from model.Task where id == id
    }!
}
