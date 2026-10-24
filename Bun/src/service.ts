import { UserRepository, TaskRepository } from "./repository";
import { User, Task } from "./types";

export class UserService {
    private repo: UserRepository;

    constructor(repo: UserRepository) {
        this.repo = repo;
    }

    createUser(name: string, email: string): User {
        return this.repo.create({ name, email });
    }

    getAllUsers(): User[] {
        return this.repo.getAll();
    }

    getUserById(id: number): User | null {
        return this.repo.getById(id);
    }

    updateUser(id: number, name: string, email: string): User | null {
        return this.repo.update(id, { name, email });
    }

    deleteUser(id: number): boolean {
        return this.repo.delete(id);
    }
}

export class TaskService {
    private taskRepo: TaskRepository;
    private userRepo: UserRepository;

    constructor(taskRepo: TaskRepository, userRepo: UserRepository) {
        this.taskRepo = taskRepo;
        this.userRepo = userRepo;
    }

    createTask(userId: number, title: string, description: string): Task {
        const user = this.userRepo.getById(userId);
        if (!user) {
            throw new Error("User not found");
        }
        return this.taskRepo.create({ user_id: userId, title, description, is_done: false });
    }

    getTasksByUserId(userId: number): Task[] {
        return this.taskRepo.getByUserId(userId);
    }

    getTaskById(id: number): Task | null {
        return this.taskRepo.getById(id);
    }

    updateTask(id: number, title: string, description: string, is_done: boolean): Task | null {
        // Need to fetch existing to know user_id if not provided, but update assumes replacement of fields?
        // Let's assume partial update is not what's asked, but the interface takes all fields.
        // However, repo update takes task object. We might need to fetch first to preserve user_id if we don't pass it.
        // But repository update query doesn't touch user_id.
        // So we can just pass the fields to update.
        // Wait, repository update needs a Task object, which has user_id.
        // But the SQL in repository `UPDATE tasks SET title = $title...` does NOT update user_id.
        // So we can pass 0 for user_id in the repo call safely if we trust the SQL.
        return this.taskRepo.update(id, { user_id: 0, title, description, is_done });
    }

    markTaskDone(id: number): Task | null {
        return this.taskRepo.markDone(id);
    }

    deleteTask(id: number): boolean {
        return this.taskRepo.delete(id);
    }
}
