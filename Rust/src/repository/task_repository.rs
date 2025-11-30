use crate::models::task::{CreateTask, Task, UpdateTask};
use sqlx::SqlitePool;
use anyhow::Result;

#[derive(Clone)]
pub struct TaskRepository {
    pool: SqlitePool,
}

impl TaskRepository {
    pub fn new(pool: SqlitePool) -> Self {
        Self { pool }
    }

    pub async fn create(&self, user_id: i64, payload: CreateTask) -> Result<Task> {
        let task = sqlx::query_as::<_, Task>(
            "INSERT INTO tasks (user_id, title, description, is_done) VALUES ($1, $2, $3, 0) RETURNING id, user_id, title, description, is_done"
        )
        .bind(user_id)
        .bind(payload.title)
        .bind(payload.description)
        .fetch_one(&self.pool)
        .await?;

        Ok(task)
    }

    pub async fn find_all_by_user(&self, user_id: i64) -> Result<Vec<Task>> {
        let tasks = sqlx::query_as::<_, Task>("SELECT * FROM tasks WHERE user_id = $1")
            .bind(user_id)
            .fetch_all(&self.pool)
            .await?;
        Ok(tasks)
    }

    pub async fn find_by_id(&self, id: i64) -> Result<Option<Task>> {
        let task = sqlx::query_as::<_, Task>("SELECT * FROM tasks WHERE id = $1")
            .bind(id)
            .fetch_optional(&self.pool)
            .await?;
        Ok(task)
    }

    pub async fn update(&self, id: i64, payload: UpdateTask) -> Result<Option<Task>> {
         let mut tx = self.pool.begin().await?;

        let current = sqlx::query_as::<_, Task>("SELECT * FROM tasks WHERE id = $1")
            .bind(id)
            .fetch_optional(&mut *tx)
            .await?;

        if let Some(task) = current {
            let title = payload.title.unwrap_or(task.title);
            let description = payload.description.unwrap_or(task.description);
            let is_done = payload.is_done.unwrap_or(task.is_done);

            let updated_task = sqlx::query_as::<_, Task>(
                "UPDATE tasks SET title = $1, description = $2, is_done = $3 WHERE id = $4 RETURNING id, user_id, title, description, is_done"
            )
            .bind(title)
            .bind(description)
            .bind(is_done)
            .bind(id)
            .fetch_one(&mut *tx)
            .await?;

            tx.commit().await?;
            Ok(Some(updated_task))
        } else {
            Ok(None)
        }
    }

    pub async fn mark_done(&self, id: i64) -> Result<Option<Task>> {
         let task = sqlx::query_as::<_, Task>(
            "UPDATE tasks SET is_done = 1 WHERE id = $1 RETURNING id, user_id, title, description, is_done"
        )
        .bind(id)
        .fetch_optional(&self.pool)
        .await?;

        Ok(task)
    }

    pub async fn delete(&self, id: i64) -> Result<bool> {
        let result = sqlx::query("DELETE FROM tasks WHERE id = $1")
            .bind(id)
            .execute(&self.pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }
}
