use std::net::SocketAddr;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use rust_magnetar::{init_db, UserRepository, TaskRepository, UserService, TaskService, AppState, app_router};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize logging
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "rust_magnetar=debug,tower_http=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    // Initialize Database
    let pool = init_db().await?;

    // Initialize Repositories
    let user_repo = UserRepository::new(pool.clone());
    let task_repo = TaskRepository::new(pool.clone());

    // Initialize Services
    let user_service = UserService::new(user_repo.clone());
    let task_service = TaskService::new(task_repo.clone(), user_repo.clone());

    let state = AppState {
        user_service,
        task_service,
    };

    // Build Router
    let app = app_router(state);

    // Run Server
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    tracing::info!("listening on {}", addr);
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
