use crate::models::task::{CreateTask, Task, UpdateTask};
use crate::repository::task_repository::TaskRepository;
use crate::repository::user_repository::UserRepository;
use anyhow::{Result, bail};

#[derive(Clone)]
pub struct TaskService {
    task_repo: TaskRepository,
    user_repo: UserRepository,
}

impl TaskService {
    pub fn new(task_repo: TaskRepository, user_repo: UserRepository) -> Self {
        Self { task_repo, user_repo }
    }

    pub async fn create_task(&self, user_id: i64, payload: CreateTask) -> Result<Task> {
        if self.user_repo.find_by_id(user_id).await?.is_none() {
            bail!("User not found");
        }

        self.task_repo.create(user_id, payload).await
    }

    pub async fn get_tasks_by_user(&self, user_id: i64) -> Result<Vec<Task>> {
         if self.user_repo.find_by_id(user_id).await?.is_none() {
            bail!("User not found");
        }
        self.task_repo.find_all_by_user(user_id).await
    }

    pub async fn get_task_by_id(&self, id: i64) -> Result<Option<Task>> {
        self.task_repo.find_by_id(id).await
    }

    pub async fn update_task(&self, id: i64, payload: UpdateTask) -> Result<Option<Task>> {
        self.task_repo.update(id, payload).await
    }

    pub async fn mark_task_done(&self, id: i64) -> Result<Option<Task>> {
        self.task_repo.mark_done(id).await
    }

    pub async fn delete_task(&self, id: i64) -> Result<bool> {
        self.task_repo.delete(id).await
    }
}
