use anyhow::{Result, anyhow};
use std::cell::RefCell;
use std::collections::HashMap;
use crate::traits::RepositoryInterface;
use crate::models::{User, Task};

pub struct MockRepository {
    pub users: RefCell<HashMap<i64, User>>,
    pub tasks: RefCell<HashMap<i64, Task>>,
    pub user_id_counter: RefCell<i64>,
    pub task_id_counter: RefCell<i64>,
}

impl MockRepository {
    pub fn new() -> Self {
        Self {
            users: RefCell::new(HashMap::new()),
            tasks: RefCell::new(HashMap::new()),
            user_id_counter: RefCell::new(1),
            task_id_counter: RefCell::new(1),
        }
    }
}

impl RepositoryInterface for MockRepository {
    fn initialize(&self) -> Result<()> {
        Ok(())
    }

    fn create_user(&self, username: &str, email: &str) -> Result<User> {
        let mut users = self.users.borrow_mut();
        let id = *self.user_id_counter.borrow();
        *self.user_id_counter.borrow_mut() += 1;
        let user = User {
            id,
            username: username.to_string(),
            email: email.to_string(),
        };
        users.insert(id, user.clone());
        Ok(user)
    }

    fn get_users(&self) -> Result<Vec<User>> {
        let users = self.users.borrow();
        Ok(users.values().cloned().collect())
    }

    fn get_user_by_id(&self, id: i64) -> Result<Option<User>> {
        let users = self.users.borrow();
        Ok(users.get(&id).cloned())
    }

    fn update_user(&self, id: i64, username: &str) -> Result<Option<User>> {
        let mut users = self.users.borrow_mut();
        if let Some(user) = users.get_mut(&id) {
            user.username = username.to_string();
            Ok(Some(user.clone()))
        } else {
            Ok(None)
        }
    }

    fn delete_user(&self, id: i64) -> Result<()> {
        let mut users = self.users.borrow_mut();
        users.remove(&id);
        Ok(())
    }

    fn create_task(&self, user_id: i64, title: &str, description: &str) -> Result<Task> {
        let mut tasks = self.tasks.borrow_mut();
        let id = *self.task_id_counter.borrow();
        *self.task_id_counter.borrow_mut() += 1;
        let task = Task {
            id,
            user_id,
            title: title.to_string(),
            description: description.to_string(),
            done: false,
        };
        tasks.insert(id, task.clone());
        Ok(task)
    }

    fn get_tasks_by_user(&self, user_id: i64) -> Result<Vec<Task>> {
        let tasks = self.tasks.borrow();
        Ok(tasks.values().filter(|t| t.user_id == user_id).cloned().collect())
    }

    fn get_task_by_id(&self, id: i64) -> Result<Option<Task>> {
        let tasks = self.tasks.borrow();
        Ok(tasks.get(&id).cloned())
    }

    fn update_task(&self, id: i64, title: Option<String>, description: Option<String>) -> Result<Option<Task>> {
        let mut tasks = self.tasks.borrow_mut();
        if let Some(task) = tasks.get_mut(&id) {
            if let Some(t) = title {
                task.title = t;
            }
            if let Some(d) = description {
                task.description = d;
            }
            Ok(Some(task.clone()))
        } else {
            Ok(None)
        }
    }

    fn update_task_done(&self, id: i64, done: bool) -> Result<Option<Task>> {
        let mut tasks = self.tasks.borrow_mut();
        if let Some(task) = tasks.get_mut(&id) {
            task.done = done;
            Ok(Some(task.clone()))
        } else {
            Ok(None)
        }
    }

    fn delete_task(&self, id: i64) -> Result<()> {
        let mut tasks = self.tasks.borrow_mut();
        tasks.remove(&id);
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::service::Service;
    use crate::models::{CreateUser, CreateTask};

    #[test]
    fn test_create_user() {
        let repo = MockRepository::new();
        let service = Service::new(repo);

        let user = service.create_user(CreateUser {
            username: "jules".to_string(),
            email: "jules@magnetar.yggdrasil".to_string(),
        }).unwrap();

        assert_eq!(user.username, "jules");
        assert_eq!(user.email, "jules@magnetar.yggdrasil");
        assert_eq!(user.id, 1);
    }

    #[test]
    fn test_create_task() {
        let repo = MockRepository::new();
        let service = Service::new(repo);

        let user = service.create_user(CreateUser {
            username: "jules".to_string(),
            email: "jules@magnetar.yggdrasil".to_string(),
        }).unwrap();

        let task = service.create_task(user.id, CreateTask {
            title: "Code".to_string(),
            description: "Write WebAssembly".to_string(),
        }).unwrap();

        assert_eq!(task.title, "Code");
        assert_eq!(task.user_id, user.id);
        assert!(!task.done);
    }

    #[test]
    fn test_mark_done() {
        let repo = MockRepository::new();
        let service = Service::new(repo);

        let user = service.create_user(CreateUser {
            username: "jules".to_string(),
            email: "jules@magnetar.yggdrasil".to_string(),
        }).unwrap();

        let task = service.create_task(user.id, CreateTask {
            title: "Code".to_string(),
            description: "Write WebAssembly".to_string(),
        }).unwrap();

        let updated = service.mark_task_done(task.id).unwrap();
        assert!(updated.done);
    }
}
