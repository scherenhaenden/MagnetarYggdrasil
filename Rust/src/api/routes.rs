use axum::{
    routing::{get, post, patch},
    Router,
};
use crate::api::handlers::{
    AppState,
    health_check,
    create_user, get_all_users, get_user_by_id, update_user, delete_user,
    create_task, get_tasks_by_user, get_task_by_id, update_task, mark_task_done, delete_task,
};

pub fn app_router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(health_check))
        // Users
        .route("/users", post(create_user).get(get_all_users))
        .route("/users/:id", get(get_user_by_id).put(update_user).delete(delete_user))
        // Tasks
        .route("/users/:id/tasks", post(create_task).get(get_tasks_by_user))
        .route("/tasks/:id", get(get_task_by_id).put(update_task).delete(delete_task))
        .route("/tasks/:id/done", patch(mark_task_done))
        .with_state(state)
}
