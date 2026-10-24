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
    /**
     * Returns a health check response.
     */
    async health(req: Request): Promise<Response> {
        return new Response(JSON.stringify({ status: "ok" }), {
            headers: { "Content-Type": "application/json" },
        });
    }

    // Users
    /**
     * Creates a new user based on the request data.
     *
     * This function processes the incoming request to extract user information, specifically the name and email.
     * If either of these fields is missing, it returns a 400 response indicating the error.
     * Upon successful creation of the user using the userService, it returns a 201 response with the user data in JSON format.
     * In case of any errors during the process, a 500 response is returned with the error message.
     *
     * @param req - The request object containing user data.
     */
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

    /**
     * Retrieves all users and returns them as a JSON response.
     */
    async getAllUsers(req: Request): Promise<Response> {
        const users = this.userService.getAllUsers();
        return new Response(JSON.stringify(users), {
            headers: { "Content-Type": "application/json" },
        });
    }

    /**
     * Retrieves a user by their ID and returns the user data or a 404 response if not found.
     */
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
    /**
     * Handles the creation of a task for a user.
     *
     * This asynchronous function processes the incoming request to create a task.
     * It attempts to extract the user ID from the request path and expects the request body
     * to contain task details. If the method is not allowed, it returns a 405 response.
     * In case of an error, it catches the exception and returns a 500 response with the error message.
     *
     * @param req - The incoming request object containing task details and user information.
     */
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

    /**
     * Retrieves a task by its ID and returns the task or a 404 response if not found.
     */
    async getTaskById(req: Request, id: number): Promise<Response> {
        const task = this.taskService.getTaskById(id);
        if (!task) {
             return new Response("Task not found", { status: 404 });
        }
        return new Response(JSON.stringify(task), {
            headers: { "Content-Type": "application/json" },
        });
    }

    /**
     * Updates a task based on the provided ID and request body.
     *
     * This asynchronous function retrieves the JSON body from the request,
     * then calls the taskService's updateTask method with the task ID and
     * the task details (title, description, and completion status). If the
     * task is not found, it returns a 404 response. In case of an error,
     * it returns a 500 response with the error message.
     *
     * @param req - The request object containing the task details in JSON format.
     * @param id - The ID of the task to be updated.
     */
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

    /**
     * Marks a task as done and returns the task details or a 404 error if not found.
     */
    async markTaskDone(req: Request, id: number): Promise<Response> {
        const task = this.taskService.markTaskDone(id);
        if (!task) {
            return new Response("Task not found", { status: 404 });
        }
        return new Response(JSON.stringify(task), {
            headers: { "Content-Type": "application/json" },
        });
    }

    /**
     * Deletes a task by its ID and returns the appropriate response.
     */
    async deleteTask(req: Request, id: number): Promise<Response> {
        const success = this.taskService.deleteTask(id);
        if (!success) {
            return new Response("Task not found", { status: 404 });
        }
        return new Response(null, { status: 204 });
    }
}
