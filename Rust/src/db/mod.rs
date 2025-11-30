use sqlx::{sqlite::SqliteConnectOptions, ConnectOptions, SqlitePool};
use std::str::FromStr;
use tracing::info;

pub async fn init_db() -> Result<SqlitePool, sqlx::Error> {
    // Create connection options
    let options = SqliteConnectOptions::from_str("sqlite://magnetar.db")?
        .create_if_missing(true)
        .journal_mode(sqlx::sqlite::SqliteJournalMode::Wal)
        .foreign_keys(true)
        .log_statements(tracing::log::LevelFilter::Debug);

    let pool = SqlitePool::connect_with(options).await?;

    info!("Running migrations...");

    // Auto-create tables
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            email TEXT NOT NULL UNIQUE
        );

        CREATE TABLE IF NOT EXISTS tasks (
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
    .await?;

    info!("Migrations complete.");

    Ok(pool)
}
