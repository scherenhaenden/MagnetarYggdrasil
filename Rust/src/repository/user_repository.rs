use crate::models::user::{CreateUser, UpdateUser, User};
use sqlx::SqlitePool;
use anyhow::Result;

#[derive(Clone)]
pub struct UserRepository {
    pool: SqlitePool,
}

impl UserRepository {
    pub fn new(pool: SqlitePool) -> Self {
        Self { pool }
    }

    pub async fn create(&self, payload: CreateUser) -> Result<User> {
        let user = sqlx::query_as::<_, User>(
            "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
        )
        .bind(payload.name)
        .bind(payload.email)
        .fetch_one(&self.pool)
        .await?;

        Ok(user)
    }

    pub async fn find_all(&self) -> Result<Vec<User>> {
        let users = sqlx::query_as::<_, User>("SELECT * FROM users")
            .fetch_all(&self.pool)
            .await?;
        Ok(users)
    }

    pub async fn find_by_id(&self, id: i64) -> Result<Option<User>> {
        let user = sqlx::query_as::<_, User>("SELECT * FROM users WHERE id = $1")
            .bind(id)
            .fetch_optional(&self.pool)
            .await?;
        Ok(user)
    }

    pub async fn update(&self, id: i64, payload: UpdateUser) -> Result<Option<User>> {
        let mut tx = self.pool.begin().await?;

        let current = sqlx::query_as::<_, User>("SELECT * FROM users WHERE id = $1")
            .bind(id)
            .fetch_optional(&mut *tx)
            .await?;

        if let Some(user) = current {
            let name = payload.name.unwrap_or(user.name);
            let email = payload.email.unwrap_or(user.email);

            let updated_user = sqlx::query_as::<_, User>(
                "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email"
            )
            .bind(name)
            .bind(email)
            .bind(id)
            .fetch_one(&mut *tx)
            .await?;

            tx.commit().await?;
            Ok(Some(updated_user))
        } else {
            Ok(None)
        }
    }

    pub async fn delete(&self, id: i64) -> Result<bool> {
        let result = sqlx::query("DELETE FROM users WHERE id = $1")
            .bind(id)
            .execute(&self.pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }
}
