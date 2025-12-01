import { Database } from "bun:sqlite";
import { User, Task } from "./types";

export class UserRepository {
    private db: Database;

    constructor(db: Database) {
        this.db = db;
    }

    /**
     * Inserts a new user into the database and returns the created user.
     */
    create(user: User): User {
        const query = this.db.query("INSERT INTO users (name, email) VALUES ($name, $email) RETURNING *");
        return query.get({ $name: user.name, $email: user.email }) as User;
    }

    /**
     * Retrieves all users from the database.
     */
    getAll(): User[] {
        return this.db.query("SELECT * FROM users").all() as User[];
    }

    /**
     * Retrieves a User by their ID from the database.
     */
    getById(id: number): User | null {
        return this.db.query("SELECT * FROM users WHERE id = $id").get({ $id: id }) as User | null;
    }

    /**
     * Updates a user's information in the database and returns the updated user or null.
     */
    update(id: number, user: User): User | null {
        const query = this.db.query("UPDATE users SET name = $name, email = $email WHERE id = $id RETURNING *");
        return query.get({ $name: user.name, $email: user.email, $id: id }) as User | null;
    }

    delete(id: number): boolean {
        const result = this.db.run("DELETE FROM users WHERE id = $id", { $id: id });
        return result.changes > 0;
    }
}

export class TaskRepository {
    private db: Database;

    constructor(db: Database) {
        this.db = db;
    }

    create(task: Task): Task {
        const query = this.db.query("INSERT INTO tasks (user_id, title, description, is_done) VALUES ($user_id, $title, $description, $is_done) RETURNING *");
        // is_done boolean needs handling? sqlite stores 0/1. bun:sqlite handles conversion?
        // Let's assume bun:sqlite handles it or we pass 0/1.
        // Actually, let's be safe and pass boolean directly, bun usually handles it.
        return query.get({
            $user_id: task.user_id,
            $title: task.title,
            $description: task.description,
            $is_done: task.is_done ? 1 : 0
        }) as Task;
    }

    getByUserId(userId: number): Task[] {
        return this.db.query("SELECT * FROM tasks WHERE user_id = $user_id").all({ $user_id: userId }) as Task[];
    }

    getById(id: number): Task | null {
        return this.db.query("SELECT * FROM tasks WHERE id = $id").get({ $id: id }) as Task | null;
    }

    update(id: number, task: Task): Task | null {
        const query = this.db.query("UPDATE tasks SET title = $title, description = $description, is_done = $is_done WHERE id = $id RETURNING *");
        return query.get({
            $title: task.title,
            $description: task.description,
            $is_done: task.is_done ? 1 : 0,
            $id: id
        }) as Task | null;
    }

    /**
     * Marks a task as done by its ID.
     */
    markDone(id: number): Task | null {
        const query = this.db.query("UPDATE tasks SET is_done = 1 WHERE id = $id RETURNING *");
        return query.get({ $id: id }) as Task | null;
    }

    /**
     * Deletes a task by its ID and returns a boolean indicating success.
     */
    delete(id: number): boolean {
        const result = this.db.run("DELETE FROM tasks WHERE id = $id", { $id: id });
        return result.changes > 0;
    }
}
