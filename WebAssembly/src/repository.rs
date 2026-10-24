use anyhow::{Result, anyhow};
use spin_sdk::sqlite::{Connection, Value};
use crate::models::{User, Task};
use crate::traits::RepositoryInterface;

pub struct Repository {
    db_name: String,
}

impl Repository {
    pub fn new(db_name: &str) -> Self {
        Self {
            db_name: db_name.to_string(),
        }
    }

    fn get_connection(&self) -> Result<Connection> {
        Connection::open(&self.db_name).map_err(|e| anyhow!("Failed to open database: {:?}", e))
    }
}

impl RepositoryInterface for Repository {
    fn initialize(&self) -> Result<()> {
        let conn = self.get_connection()?;

        conn.execute("PRAGMA foreign_keys = ON;", &[])
            .map_err(|e| anyhow!("Failed to enable foreign keys: {:?}", e))?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS users (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                username TEXT UNIQUE NOT NULL,
                email TEXT UNIQUE NOT NULL
            );",
            &[],
        ).map_err(|e| anyhow!("Failed to create users table: {:?}", e))?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS tasks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                user_id INTEGER NOT NULL,
                title TEXT NOT NULL,
                description TEXT NOT NULL,
                done BOOLEAN NOT NULL DEFAULT 0,
                FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
            );",
            &[],
        ).map_err(|e| anyhow!("Failed to create tasks table: {:?}", e))?;

        Ok(())
    }

    // User Operations

    fn create_user(&self, username: &str, email: &str) -> Result<User> {
        let conn = self.get_connection()?;

        // Check for duplicates handled by unique constraint, but good to catch
        conn.execute(
            "INSERT INTO users (username, email) VALUES (?, ?)",
            &[Value::Text(username.to_string()), Value::Text(email.to_string())],
        ).map_err(|e| anyhow!("Failed to insert user: {:?}", e))?;

        // SQLite via Spin doesn't have last_insert_rowid easily exposed in all versions,
        // but let's assume we can query it or it works.
        // Actually, spin_sdk::sqlite 0.1+ supports standard queries.
        // We can select the user back.

        let result = conn.execute("SELECT id, username, email FROM users WHERE username = ?", &[Value::Text(username.to_string())])
             .map_err(|e| anyhow!("Failed to fetch created user: {:?}", e))?;

        let user_result = if let Some(row) = result.rows().next() {
            let id = row.get::<i64>("id").ok_or(anyhow!("Missing id"))?;
            let username = row.get::<&str>("username").ok_or(anyhow!("Missing username"))?.to_string();
            let email = row.get::<&str>("email").ok_or(anyhow!("Missing email"))?.to_string();

            Ok(User { id, username, email })
        } else {
             Err(anyhow!("User not found after creation"))
        };
        user_result
    }

    fn get_users(&self) -> Result<Vec<User>> {
        let conn = self.get_connection()?;
        let result = conn.execute("SELECT id, username, email FROM users", &[])
            .map_err(|e| anyhow!("Failed to fetch users: {:?}", e))?;

        let mut users = Vec::new();
        for row in result.rows() {
            let id = row.get::<i64>("id").ok_or(anyhow!("Missing id"))?;
            let username = row.get::<&str>("username").ok_or(anyhow!("Missing username"))?.to_string();
            let email = row.get::<&str>("email").ok_or(anyhow!("Missing email"))?.to_string();
            users.push(User { id, username, email });
        }
        Ok(users)
    }

    fn get_user_by_id(&self, id: i64) -> Result<Option<User>> {
        let conn = self.get_connection()?;
        let result = conn.execute("SELECT id, username, email FROM users WHERE id = ?", &[Value::Integer(id)])
            .map_err(|e| anyhow!("Failed to fetch user: {:?}", e))?;

        let user_result = if let Some(row) = result.rows().next() {
             let id = row.get::<i64>("id").ok_or(anyhow!("Missing id"))?;
             let username = row.get::<&str>("username").ok_or(anyhow!("Missing username"))?.to_string();
             let email = row.get::<&str>("email").ok_or(anyhow!("Missing email"))?.to_string();
             Ok(Some(User { id, username, email }))
        } else {
            Ok(None)
        };
        user_result
    }

    fn update_user(&self, id: i64, username: &str) -> Result<Option<User>> {
        let conn = self.get_connection()?;
        let _ = conn.execute(
            "UPDATE users SET username = ? WHERE id = ?",
            &[Value::Text(username.to_string()), Value::Integer(id)],
        ).map_err(|e| anyhow!("Failed to update user: {:?}", e))?;

        // Verify update happened
        // Ideally we check affected rows but spin sdk result might vary.
        // Let's just fetch.
        self.get_user_by_id(id)
    }

    fn delete_user(&self, id: i64) -> Result<()> {
        let conn = self.get_connection()?;
        conn.execute("DELETE FROM users WHERE id = ?", &[Value::Integer(id)])
            .map_err(|e| anyhow!("Failed to delete user: {:?}", e))?;
        Ok(())
    }

    // Task Operations

    fn create_task(&self, user_id: i64, title: &str, description: &str) -> Result<Task> {
        let conn = self.get_connection()?;
        conn.execute(
            "INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0)",
            &[Value::Integer(user_id), Value::Text(title.to_string()), Value::Text(description.to_string())],
        ).map_err(|e| anyhow!("Failed to insert task: {:?}", e))?;

        // Fetch back using last_insert_rowid equivalent or finding max id for user?
        // SQLite supports `last_insert_rowid()`.
        let result = conn.execute("SELECT id, user_id, title, description, done FROM tasks WHERE rowid = last_insert_rowid()", &[])
            .map_err(|e| anyhow!("Failed to fetch created task: {:?}", e))?;

         let task_result = if let Some(row) = result.rows().next() {
            let id = row.get::<i64>("id").ok_or(anyhow!("Missing id"))?;
            let user_id = row.get::<i64>("user_id").ok_or(anyhow!("Missing user_id"))?;
            let title = row.get::<&str>("title").ok_or(anyhow!("Missing title"))?.to_string();
            let description = row.get::<&str>("description").ok_or(anyhow!("Missing description"))?.to_string();
            let done = row.get::<bool>("done").or(Some(false)).unwrap(); // bool handling might differ

            Ok(Task { id, user_id, title, description, done })
        } else {
             Err(anyhow!("Task not found after creation"))
        };
        task_result
    }

    fn get_tasks_by_user(&self, user_id: i64) -> Result<Vec<Task>> {
        let conn = self.get_connection()?;
        let result = conn.execute("SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?", &[Value::Integer(user_id)])
            .map_err(|e| anyhow!("Failed to fetch tasks: {:?}", e))?;

        let mut tasks = Vec::new();
        for row in result.rows() {
            let id = row.get::<i64>("id").ok_or(anyhow!("Missing id"))?;
            let user_id = row.get::<i64>("user_id").ok_or(anyhow!("Missing user_id"))?;
            let title = row.get::<&str>("title").ok_or(anyhow!("Missing title"))?.to_string();
            let description = row.get::<&str>("description").ok_or(anyhow!("Missing description"))?.to_string();
            let done = row.get::<bool>("done").unwrap_or(false);
            tasks.push(Task { id, user_id, title, description, done });
        }
        Ok(tasks)
    }

    fn get_task_by_id(&self, id: i64) -> Result<Option<Task>> {
        let conn = self.get_connection()?;
        let result = conn.execute("SELECT id, user_id, title, description, done FROM tasks WHERE id = ?", &[Value::Integer(id)])
            .map_err(|e| anyhow!("Failed to fetch task: {:?}", e))?;

         let task_result = if let Some(row) = result.rows().next() {
            let id = row.get::<i64>("id").ok_or(anyhow!("Missing id"))?;
            let user_id = row.get::<i64>("user_id").ok_or(anyhow!("Missing user_id"))?;
            let title = row.get::<&str>("title").ok_or(anyhow!("Missing title"))?.to_string();
            let description = row.get::<&str>("description").ok_or(anyhow!("Missing description"))?.to_string();
            let done = row.get::<bool>("done").unwrap_or(false);
            Ok(Some(Task { id, user_id, title, description, done }))
        } else {
            Ok(None)
        };
        task_result
    }

    fn update_task(&self, id: i64, title: Option<String>, description: Option<String>) -> Result<Option<Task>> {
        let conn = self.get_connection()?;

        if let Some(t) = title.as_ref() {
             conn.execute("UPDATE tasks SET title = ? WHERE id = ?", &[Value::Text(t.clone()), Value::Integer(id)])?;
        }
        if let Some(d) = description.as_ref() {
             conn.execute("UPDATE tasks SET description = ? WHERE id = ?", &[Value::Text(d.clone()), Value::Integer(id)])?;
        }

        self.get_task_by_id(id)
    }

    fn update_task_done(&self, id: i64, done: bool) -> Result<Option<Task>> {
        let conn = self.get_connection()?;
        conn.execute("UPDATE tasks SET done = ? WHERE id = ?", &[Value::Integer(if done { 1 } else { 0 }), Value::Integer(id)])?;
        self.get_task_by_id(id)
    }

    fn delete_task(&self, id: i64) -> Result<()> {
        let conn = self.get_connection()?;
        conn.execute("DELETE FROM tasks WHERE id = ?", &[Value::Integer(id)])
            .map_err(|e| anyhow!("Failed to delete task: {:?}", e))?;
        Ok(())
    }
}
