module app;

import vibe.d;
import db;
import controller;

void main()
{
	auto settings = new HTTPServerSettings;
	settings.port = 8080;
	settings.bindAddresses = ["::1", "127.0.0.1"];

	auto router = new URLRouter;

	// Health check
	router.get("/health", (req, res) { res.writeBody("OK"); });

	// Users
	router.post("/users", &createUserHandler);
	router.get("/users", &getAllUsersHandler);
	router.get("/users/:id", &getUserHandler);
	router.put("/users/:id", &updateUserHandler);
	router.delete("/users/:id", &deleteUserHandler);

	// Tasks
	router.post("/users/:id/tasks", &createTaskHandler);
	router.get("/users/:id/tasks", &getTasksByUserHandler);
	router.get("/tasks/:tid", &getTaskHandler);
	router.put("/tasks/:tid", &updateTaskHandler);
	router.patch("/tasks/:tid/done", &markTaskDoneHandler);
	router.delete("/tasks/:tid", &deleteTaskHandler);

    // Initialize DB
    // Vibe.d runs in fibers, but initDB is blocking. That's fine for startup.
    // The DB path is standard for this project?
    // README says "SQLite file auto-creates".
    // I'll name it "magnetar.db" in the current directory or ./data/magnetar.db
    // I'll put it in current dir for now.

    initDB("magnetar.db");

	listenHTTP(settings, router);

	logInfo("Please open http://127.0.0.1:8080/ in your browser.");
	runApplication();
}
