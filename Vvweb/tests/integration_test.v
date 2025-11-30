module tests

import os
import time
import net.http
import json
import db.sqlite

struct User {
	id       int
	username string
	email    string
}

struct Task {
	id          int
	user_id     int
	title       string
	description string
	done        bool
}

fn test_integration() {
    // This is a basic integration test placeholder.
    // In a real scenario, we would start the server in a separate process or thread
    // and run HTTP requests against it.
    // However, V's testing framework usually runs unit tests.
    // For full integration/e2e, we often use a script.

    // We can test the repository layer here if we want.
    test_repository()!
}

fn test_repository() ! {
    db_path := ':memory:'
    db := sqlite.connect(db_path)!

    // Create tables manually for test or use repository init logic if accessible
    db.exec('PRAGMA foreign_keys = ON;')!
	db.exec('CREATE TABLE IF NOT EXISTS users (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		username TEXT UNIQUE NOT NULL,
		email TEXT UNIQUE NOT NULL
	)')!
	db.exec('CREATE TABLE IF NOT EXISTS tasks (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		user_id INTEGER NOT NULL,
		title TEXT NOT NULL,
		description TEXT NOT NULL,
		done BOOLEAN DEFAULT 0,
		FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
	)')!

    // Insert User
    db.exec("INSERT INTO users (username, email) VALUES ('testuser', 'test@example.com')")!

    users := db.exec('SELECT * FROM users')!
    assert users.len == 1
    assert users[0].vals[1] == 'testuser'

    println('Repository test passed')
}
