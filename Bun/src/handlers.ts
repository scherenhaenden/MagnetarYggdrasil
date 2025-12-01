import { UserService, TaskService } from "./service";
import { User } from "./types";

export class Handlers {
    private userService: UserService;
    private taskService: TaskService;

    constructor(userService: UserService, taskService: TaskService) {
        this.userService = userService;
        this.taskService = taskService;
    }

    // Health
    async health(req: Request): Promise<Response> {
        return new Response(JSON.stringify({ status: "ok" }), {
            headers: { "Content-Type": "application/json" },
        });
    }

    // Users
    async createUser(req: Request): Promise<Response> {
        try {
            const body = await req.json() as any;
            if (!body.name || !body.email) {
                return new Response("Missing name or email", { status: 400 });
            }
            const user = this.userService.createUser(body.name, body.email);
            return new Response(JSON.stringify(user), {
                status: 201,
                headers: { "Content-Type": "application/json" },
            });
        } catch (e: any) {
            return new Response(e.message, { status: 500 });
        }
    }

    async getAllUsers(req: Request): Promise<Response> {
        const users = this.userService.getAllUsers();
        return new Response(JSON.stringify(users), {
            headers: { "Content-Type": "application/json" },
        });
    }

    async getUserById(req: Request, id: number): Promise<Response> {
        const user = this.userService.getUserById(id);
        if (!user) {
            return new Response("User not found", { status: 404 });
        }
        return new Response(JSON.stringify(user), {
            headers: { "Content-Type": "application/json" },
        });
    }

    async updateUser(req: Request, id: number): Promise<Response> {
        try {
            const body = await req.json() as any;
            const user = this.userService.updateUser(id, body.name, body.email);
            if (!user) {
                return new Response("User not found", { status: 404 });
            }
            return new Response(JSON.stringify(user), {
                headers: { "Content-Type": "application/json" },
            });
        } catch (e: any) {
            return new Response(e.message, { status: 500 });
        }
    }

    async deleteUser(req: Request, id: number): Promise<Response> {
        const success = this.userService.deleteUser(id);
        if (!success) {
            return new Response("User not found", { status: 404 });
        }
        return new Response(null, { status: 204 });
    }

    // Tasks
    async createTask(req: Request): Promise<Response> {
        try {
            const body = await req.json() as any;
            // Expected body: { user_id, title, description }?
            // Or maybe path param?
            // The Rust implementation used: POST /users/:id/tasks (implied) or passed user_id in body?
            // Rust test: `POST /users` then `POST /users/:id/tasks` usually?
            // Rust route: `.route("/users/:id/tasks", post(create_task)`
            // So user_id is in the path.

            // Wait, looking at Rust routes again:
            // .route("/users/:id/tasks", post(create_task).get(get_tasks_by_user))
            // So for create_task, user_id comes from path.

            // But here I need to separate the method because I'm manually routing or using a router.
            // I'll update the signature to accept userId.

            // Wait, let's look at `createTask` signature in this class.
            // I will implement `createTaskForUser` which takes userId from path.

            return new Response("Method Not Allowed", { status: 405 });
        } catch (e: any) {
            return new Response(e.message, { status: 500 });
        }
    }

    async createTaskForUser(req: Request, userId: number): Promise<Response> {
        try {
            const body = await req.json() as any;
             if (!body.title || !body.description) {
                return new Response("Missing title or description", { status: 400 });
            }
            const task = this.taskService.createTask(userId, body.title, body.description);
            return new Response(JSON.stringify(task), {
                status: 201,
                headers: { "Content-Type": "application/json" },
            });
        } catch (e: any) {
            if (e.message === "User not found") {
                return new Response(e.message, { status: 404 });
            }
             return new Response(e.message, { status: 500 });
        }
    }

    async getTasksByUserId(req: Request, userId: number): Promise<Response> {
        const tasks = this.taskService.getTasksByUserId(userId);
        return new Response(JSON.stringify(tasks), {
            headers: { "Content-Type": "application/json" },
        });
    }

    async getTaskById(req: Request, id: number): Promise<Response> {
        const task = this.taskService.getTaskById(id);
        if (!task) {
             return new Response("Task not found", { status: 404 });
        }
        return new Response(JSON.stringify(task), {
            headers: { "Content-Type": "application/json" },
        });
    }

    async updateTask(req: Request, id: number): Promise<Response> {
        try {
            const body = await req.json() as any;
            const task = this.taskService.updateTask(id, body.title, body.description, body.is_done);
            if (!task) {
                return new Response("Task not found", { status: 404 });
            }
            return new Response(JSON.stringify(task), {
                headers: { "Content-Type": "application/json" },
            });
        } catch (e: any) {
            return new Response(e.message, { status: 500 });
        }
    }

    async markTaskDone(req: Request, id: number): Promise<Response> {
        const task = this.taskService.markTaskDone(id);
        if (!task) {
            return new Response("Task not found", { status: 404 });
        }
        return new Response(JSON.stringify(task), {
            headers: { "Content-Type": "application/json" },
        });
    }

    async deleteTask(req: Request, id: number): Promise<Response> {
        const success = this.taskService.deleteTask(id);
        if (!success) {
            return new Response("Task not found", { status: 404 });
        }
        return new Response(null, { status: 204 });
    }
}
