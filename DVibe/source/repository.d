module repository;

import model;
import db;
import d2sqlite3;
import std.typecons;
import std.array;

// Users Repository

User[] getAllUsers() {
    auto db = getDB();
    auto stmt = db.prepare("SELECT id, name, email FROM users");
    User[] users;
    foreach (Row row; stmt.execute()) {
        users ~= User(row.peek!long(0), row.peek!string(1), row.peek!string(2));
    }
    return users;
}

Nullable!User getUserById(long id) {
    auto db = getDB();
    auto stmt = db.prepare("SELECT id, name, email FROM users WHERE id = :id");
    stmt.bind(":id", id);
    auto results = stmt.execute();
    if (results.empty) return Nullable!User();
    Row row = results.one();
    return Nullable!User(User(row.peek!long(0), row.peek!string(1), row.peek!string(2)));
}

long createUser(string name, string email) {
    auto db = getDB();
    auto stmt = db.prepare("INSERT INTO users (name, email) VALUES (:name, :email)");
    stmt.bind(":name", name);
    stmt.bind(":email", email);
    stmt.execute();
    return db.lastInsertRowid();
}

void updateUser(long id, string name, string email) {
    auto db = getDB();
    string sql = "UPDATE users SET ";
    bool first = true;
    if (name !is null) {
        sql ~= "name = :name";
        first = false;
    }
    if (email !is null) {
        if (!first) sql ~= ", ";
        sql ~= "email = :email";
    }
    // If nothing to update, just return
    if (first) return;

    sql ~= " WHERE id = :id";

    auto stmt = db.prepare(sql);
    if (name !is null) stmt.bind(":name", name);
    if (email !is null) stmt.bind(":email", email);
    stmt.bind(":id", id);
    stmt.execute();
}

void deleteUser(long id) {
    auto db = getDB();
    auto stmt = db.prepare("DELETE FROM users WHERE id = :id");
    stmt.bind(":id", id);
    stmt.execute();
}

bool userExists(long id) {
    auto db = getDB();
    auto stmt = db.prepare("SELECT 1 FROM users WHERE id = :id");
    stmt.bind(":id", id);
    return !stmt.execute().empty;
}

// Tasks Repository

Task[] getTasksByUserId(long userId) {
    auto db = getDB();
    auto stmt = db.prepare("SELECT id, user_id, title, description, done FROM tasks WHERE user_id = :uid");
    stmt.bind(":uid", userId);
    Task[] tasks;
    foreach (Row row; stmt.execute()) {
        tasks ~= Task(
            row.peek!long(0),
            row.peek!long(1),
            row.peek!string(2),
            row.peek!string(3),
            row.peek!bool(4)
        );
    }
    return tasks;
}

Nullable!Task getTaskById(long id) {
    auto db = getDB();
    auto stmt = db.prepare("SELECT id, user_id, title, description, done FROM tasks WHERE id = :id");
    stmt.bind(":id", id);
    auto results = stmt.execute();
    if (results.empty) return Nullable!Task();
    Row row = results.one();
    return Nullable!Task(Task(
        row.peek!long(0),
        row.peek!long(1),
        row.peek!string(2),
        row.peek!string(3),
        row.peek!bool(4)
    ));
}

long createTask(long userId, string title, string description) {
    auto db = getDB();
    auto stmt = db.prepare("INSERT INTO tasks (user_id, title, description, done) VALUES (:uid, :title, :desc, 0)");
    stmt.bind(":uid", userId);
    stmt.bind(":title", title);
    stmt.bind(":desc", description);
    stmt.execute();
    return db.lastInsertRowid();
}

void updateTask(long id, string title, string description, Nullable!bool done) {
    auto db = getDB();
    // Construct query dynamically
    string sql = "UPDATE tasks SET ";
    bool first = true;
    if (title !is null) {
        sql ~= "title = :title";
        first = false;
    }
    if (description !is null) {
        if (!first) sql ~= ", ";
        sql ~= "description = :desc";
        first = false;
    }
    if (!done.isNull) {
        if (!first) sql ~= ", ";
        sql ~= "done = :done";
    }

    if (first) return;

    sql ~= " WHERE id = :id";

    auto stmt = db.prepare(sql);
    if (title !is null) stmt.bind(":title", title);
    if (description !is null) stmt.bind(":desc", description);
    if (!done.isNull) stmt.bind(":done", done.get());
    stmt.bind(":id", id);
    stmt.execute();
}

void markTaskDone(long id) {
    auto db = getDB();
    auto stmt = db.prepare("UPDATE tasks SET done = 1 WHERE id = :id");
    stmt.bind(":id", id);
    stmt.execute();
}

void deleteTask(long id) {
    auto db = getDB();
    auto stmt = db.prepare("DELETE FROM tasks WHERE id = :id");
    stmt.bind(":id", id);
    stmt.execute();
}

version(unittest) {
    import std.file;
    import std.stdio;

    shared static this() {
        if (exists("test_repo.db")) remove("test_repo.db");
        initDB("test_repo.db");
    }

    unittest {
        writeln("Running Repository Tests...");
        getDB().run("DELETE FROM tasks");
        getDB().run("DELETE FROM users");

        // Test User Repo
        long uid = createUser("Repo User", "repo@example.com");
        assert(uid > 0);

        auto user = getUserById(uid);
        assert(!user.isNull);
        assert(user.get().name == "Repo User");

        updateUser(uid, "New Name", null);
        user = getUserById(uid);
        assert(user.get().name == "New Name");
        assert(user.get().email == "repo@example.com");

        // Test Task Repo
        long tid = createTask(uid, "Repo Task", "Repo Desc");
        assert(tid > 0);

        auto task = getTaskById(tid);
        assert(!task.isNull);
        assert(task.get().title == "Repo Task");
        assert(task.get().done == false);

        updateTask(tid, null, null, Nullable!bool(true));
        task = getTaskById(tid);
        assert(task.get().done == true);

        deleteTask(tid);
        assert(getTaskById(tid).isNull);

        deleteUser(uid);
        assert(getUserById(uid).isNull);
    }
}
