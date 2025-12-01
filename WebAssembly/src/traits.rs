use anyhow::Result;
use crate::models::{User, Task};

pub trait RepositoryInterface {
    fn initialize(&self) -> Result<()>;
    fn create_user(&self, username: &str, email: &str) -> Result<User>;
    fn get_users(&self) -> Result<Vec<User>>;
    fn get_user_by_id(&self, id: i64) -> Result<Option<User>>;
    fn update_user(&self, id: i64, username: &str) -> Result<Option<User>>;
    fn delete_user(&self, id: i64) -> Result<()>;
    fn create_task(&self, user_id: i64, title: &str, description: &str) -> Result<Task>;
    fn get_tasks_by_user(&self, user_id: i64) -> Result<Vec<Task>>;
    fn get_task_by_id(&self, id: i64) -> Result<Option<Task>>;
    fn update_task(&self, id: i64, title: Option<String>, description: Option<String>) -> Result<Option<Task>>;
    fn update_task_done(&self, id: i64, done: bool) -> Result<Option<Task>>;
    fn delete_task(&self, id: i64) -> Result<()>;
}
