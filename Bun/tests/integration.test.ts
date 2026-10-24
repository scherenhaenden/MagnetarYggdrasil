import { describe, expect, test, beforeAll, afterAll } from "bun:test";
import { DB } from "../src/db";
import { UserRepository, TaskRepository } from "../src/repository";
import { UserService, TaskService } from "../src/service";
import { Handlers } from "../src/handlers";
import { Router } from "../src/router";

describe("Integration Tests", () => {
    let db: DB;
    let router: Router;

    beforeAll(() => {
        db = new DB(":memory:"); // Use in-memory DB for tests
        const userRepo = new UserRepository(db.getDatabase());
        const taskRepo = new TaskRepository(db.getDatabase());
        const userService = new UserService(userRepo);
        const taskService = new TaskService(taskRepo, userRepo);
        const handlers = new Handlers(userService, taskService);
        router = new Router(handlers);
    });

    afterAll(() => {
        db.close();
    });

    test("Health Check", async () => {
        const req = new Request("http://localhost/health");
        const res = await router.handle(req);
        expect(res.status).toBe(200);
        const body = await res.json();
        expect(body.status).toBe("ok");
    });

    test("Create User", async () => {
        const req = new Request("http://localhost/users", {
            method: "POST",
            body: JSON.stringify({ name: "Test User", email: "test@example.com" }),
        });
        const res = await router.handle(req);
        expect(res.status).toBe(201);
        const body = await res.json() as any;
        expect(body.name).toBe("Test User");
        expect(body.email).toBe("test@example.com");
        expect(body.id).toBeDefined();
    });

    test("Get All Users", async () => {
        const req = new Request("http://localhost/users");
        const res = await router.handle(req);
        expect(res.status).toBe(200);
        const body = await res.json() as any[];
        expect(body.length).toBeGreaterThan(0);
    });

    test("Get User By ID", async () => {
         // First create a user
        const createReq = new Request("http://localhost/users", {
            method: "POST",
            body: JSON.stringify({ name: "User 2", email: "user2@example.com" }),
        });
        const createRes = await router.handle(createReq);
        const createdUser = await createRes.json() as any;

        const req = new Request(`http://localhost/users/${createdUser.id}`);
        const res = await router.handle(req);
        expect(res.status).toBe(200);
        const body = await res.json() as any;
        expect(body.id).toBe(createdUser.id);
    });

    test("Create Task", async () => {
        // Create user first
        const createReq = new Request("http://localhost/users", {
            method: "POST",
            body: JSON.stringify({ name: "Task User", email: "task@example.com" }),
        });
        const createRes = await router.handle(createReq);
        const user = await createRes.json() as any;

        const taskReq = new Request(`http://localhost/users/${user.id}/tasks`, {
            method: "POST",
            body: JSON.stringify({ title: "Test Task", description: "Do it" }),
        });
        const taskRes = await router.handle(taskReq);
        expect(taskRes.status).toBe(201);
        const task = await taskRes.json() as any;
        expect(task.title).toBe("Test Task");
        expect(task.user_id).toBe(user.id);
    });

    test("Mark Task Done", async () => {
         // Create user
        const createReq = new Request("http://localhost/users", {
            method: "POST",
            body: JSON.stringify({ name: "Done User", email: "done@example.com" }),
        });
        const createRes = await router.handle(createReq);
        const user = await createRes.json() as any;

        // Create task
        const taskReq = new Request(`http://localhost/users/${user.id}/tasks`, {
            method: "POST",
            body: JSON.stringify({ title: "Task to complete", description: "..." }),
        });
        const taskRes = await router.handle(taskReq);
        const task = await taskRes.json() as any;

        // Mark done
        const doneReq = new Request(`http://localhost/tasks/${task.id}/done`, {
            method: "PATCH"
        });
        const doneRes = await router.handle(doneReq);
        expect(doneRes.status).toBe(200);
        const doneTask = await doneRes.json() as any;
        expect(doneTask.is_done).toBe(1); // SQLite returns 1 for true
    });
});
