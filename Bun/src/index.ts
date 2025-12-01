import { DB } from "./db";
import { UserRepository, TaskRepository } from "./repository";
import { UserService, TaskService } from "./service";
import { Handlers } from "./handlers";
import { Router } from "./router";

const db = new DB("magnetar.sqlite");
const userRepo = new UserRepository(db.getDatabase());
const taskRepo = new TaskRepository(db.getDatabase());
const userService = new UserService(userRepo);
const taskService = new TaskService(taskRepo, userRepo);
const handlers = new Handlers(userService, taskService);
const router = new Router(handlers);

console.log("Listening on http://localhost:3000 ...");

Bun.serve({
  port: 3000,
  fetch(req) {
    return router.handle(req);
  },
});
