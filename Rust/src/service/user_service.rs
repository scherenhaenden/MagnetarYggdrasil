use crate::models::user::{CreateUser, UpdateUser, User};
use crate::repository::user_repository::UserRepository;
use anyhow::Result;

#[derive(Clone)]
pub struct UserService {
    repo: UserRepository,
}

impl UserService {
    pub fn new(repo: UserRepository) -> Self {
        Self { repo }
    }

    pub async fn create_user(&self, payload: CreateUser) -> Result<User> {
        self.repo.create(payload).await
    }

    pub async fn get_all_users(&self) -> Result<Vec<User>> {
        self.repo.find_all().await
    }

    pub async fn get_user_by_id(&self, id: i64) -> Result<Option<User>> {
        self.repo.find_by_id(id).await
    }

    pub async fn update_user(&self, id: i64, payload: UpdateUser) -> Result<Option<User>> {
        self.repo.update(id, payload).await
    }

    pub async fn delete_user(&self, id: i64) -> Result<bool> {
        self.repo.delete(id).await
    }
}
