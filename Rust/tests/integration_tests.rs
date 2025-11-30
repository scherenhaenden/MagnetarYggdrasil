use axum::{
    body::Body,
    http::{Request, StatusCode},
};
use tower::util::ServiceExt;
use serde_json::{json, Value};
use sqlx::{SqlitePool, sqlite::SqliteConnectOptions};
use std::str::FromStr;
use rust_magnetar::{app_router, AppState, UserRepository, TaskRepository, UserService, TaskService};

async fn setup_test_db() -> SqlitePool {
    let options = SqliteConnectOptions::from_str("sqlite::memory:")
        .unwrap()
        .create_if_missing(true)
        .foreign_keys(true);

    let pool = SqlitePool::connect_with(options).await.unwrap();

    // Run migrations
    sqlx::query(
        r#"
        CREATE TABLE users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            email TEXT NOT NULL UNIQUE
        );

        CREATE TABLE tasks (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            title TEXT NOT NULL,
            description TEXT NOT NULL,
            is_done BOOLEAN NOT NULL DEFAULT 0,
            FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
        );
        "#
    )
    .execute(&pool)
    .await
    .unwrap();

    pool
}

#[tokio::test]
async fn test_health_check() {
    let pool = setup_test_db().await;
    let user_repo = UserRepository::new(pool.clone());
    let task_repo = TaskRepository::new(pool.clone());
    let user_service = UserService::new(user_repo.clone());
    let task_service = TaskService::new(task_repo, user_repo);
    let state = AppState { user_service, task_service };
    let app = app_router(state);

    let response = app
        .oneshot(Request::builder().uri("/health").body(Body::empty()).unwrap())
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_create_user() {
    let pool = setup_test_db().await;
    let user_repo = UserRepository::new(pool.clone());
    let task_repo = TaskRepository::new(pool.clone());
    let user_service = UserService::new(user_repo.clone());
    let task_service = TaskService::new(task_repo, user_repo);
    let state = AppState { user_service, task_service };
    let app = app_router(state);

    let response = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/users")
                .header("Content-Type", "application/json")
                .body(Body::from(json!({ "name": "Test User", "email": "test@example.com" }).to_string()))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX).await.unwrap();
    let body: Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(body["name"], "Test User");
    assert_eq!(body["email"], "test@example.com");
}
