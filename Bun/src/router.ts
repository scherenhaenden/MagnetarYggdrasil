import { Handlers } from "./handlers";

export class Router {
    private handlers: Handlers;

    constructor(handlers: Handlers) {
        this.handlers = handlers;
    }

    async handle(req: Request): Promise<Response> {
        const url = new URL(req.url);
        const path = url.pathname;
        const method = req.method;

        // Health
        if (path === "/health" && method === "GET") {
            return this.handlers.health(req);
        }

        // Users
        if (path === "/users" && method === "POST") {
            return this.handlers.createUser(req);
        }
        if (path === "/users" && method === "GET") {
            return this.handlers.getAllUsers(req);
        }

        // Match /users/:id
        const usersIdMatch = path.match(/^\/users\/(\d+)$/);
        if (usersIdMatch) {
            const id = parseInt(usersIdMatch[1]);
            if (method === "GET") return this.handlers.getUserById(req, id);
            if (method === "PUT") return this.handlers.updateUser(req, id);
            if (method === "DELETE") return this.handlers.deleteUser(req, id);
        }

        // Match /users/:id/tasks
        const userTasksMatch = path.match(/^\/users\/(\d+)\/tasks$/);
        if (userTasksMatch) {
            const userId = parseInt(userTasksMatch[1]);
            if (method === "POST") return this.handlers.createTaskForUser(req, userId);
            if (method === "GET") return this.handlers.getTasksByUserId(req, userId);
        }

        // Match /tasks/:id
        const tasksIdMatch = path.match(/^\/tasks\/(\d+)$/);
        if (tasksIdMatch) {
            const id = parseInt(tasksIdMatch[1]);
            if (method === "GET") return this.handlers.getTaskById(req, id);
            if (method === "PUT") return this.handlers.updateTask(req, id);
            if (method === "DELETE") return this.handlers.deleteTask(req, id);
        }

        // Match /tasks/:id/done
        const tasksDoneMatch = path.match(/^\/tasks\/(\d+)\/done$/);
        if (tasksDoneMatch) {
            const id = parseInt(tasksDoneMatch[1]);
            if (method === "PATCH") return this.handlers.markTaskDone(req, id);
        }

        return new Response("Not Found", { status: 404 });
    }
}
