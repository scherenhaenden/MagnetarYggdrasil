use anyhow::{Result, anyhow};
use crate::models::{User, CreateUser, UpdateUser, Task, CreateTask, UpdateTask};
use crate::traits::RepositoryInterface;

pub struct Service<R: RepositoryInterface> {
    repo: R,
}

impl<R: RepositoryInterface> Service<R> {
    pub fn new(repo: R) -> Self {
        Self { repo }
    }

    pub fn initialize_db(&self) -> Result<()> {
        self.repo.initialize()
    }

    // User Services

    pub fn create_user(&self, payload: CreateUser) -> Result<User> {
        self.repo.create_user(&payload.username, &payload.email)
    }

    pub fn get_users(&self) -> Result<Vec<User>> {
        self.repo.get_users()
    }

    pub fn get_user(&self, id: i64) -> Result<User> {
        self.repo.get_user_by_id(id)?
            .ok_or_else(|| anyhow!("User not found"))
    }

    pub fn update_user(&self, id: i64, payload: UpdateUser) -> Result<User> {
        let user = self.repo.get_user_by_id(id)?.ok_or_else(|| anyhow!("User not found"))?;

        let new_username = payload.username.unwrap_or(user.username);

        self.repo.update_user(id, &new_username)?
            .ok_or_else(|| anyhow!("User not found after update"))
    }

    pub fn delete_user(&self, id: i64) -> Result<()> {
        self.repo.get_user_by_id(id)?.ok_or_else(|| anyhow!("User not found"))?;
        self.repo.delete_user(id)
    }

    // Task Services

    pub fn create_task(&self, user_id: i64, payload: CreateTask) -> Result<Task> {
        // Ensure user exists
        self.repo.get_user_by_id(user_id)?.ok_or_else(|| anyhow!("User not found"))?;

        self.repo.create_task(user_id, &payload.title, &payload.description)
    }

    pub fn get_tasks(&self, user_id: i64) -> Result<Vec<Task>> {
         // Ensure user exists
        self.repo.get_user_by_id(user_id)?.ok_or_else(|| anyhow!("User not found"))?;

        self.repo.get_tasks_by_user(user_id)
    }

    pub fn get_task(&self, id: i64) -> Result<Task> {
        self.repo.get_task_by_id(id)?
            .ok_or_else(|| anyhow!("Task not found"))
    }

    pub fn update_task(&self, id: i64, payload: UpdateTask) -> Result<Task> {
        self.repo.get_task_by_id(id)?.ok_or_else(|| anyhow!("Task not found"))?;

        self.repo.update_task(id, payload.title, payload.description)?
             .ok_or_else(|| anyhow!("Task not found after update"))
    }

    pub fn mark_task_done(&self, id: i64) -> Result<Task> {
        self.repo.get_task_by_id(id)?.ok_or_else(|| anyhow!("Task not found"))?;

        self.repo.update_task_done(id, true)?
            .ok_or_else(|| anyhow!("Task not found after update"))
    }

    pub fn delete_task(&self, id: i64) -> Result<()> {
        self.repo.get_task_by_id(id)?.ok_or_else(|| anyhow!("Task not found"))?;
        self.repo.delete_task(id)
    }
}
