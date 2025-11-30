module controller;

import vibe.d;
import service;
import model;

// Users

void createUserHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        auto createReq = req.json.deserializeJson!CreateUserRequest;
        auto user = createUserService(createReq);
        res.writeJsonBody(user, HTTPStatus.created);
    } catch (ServiceException e) {
        res.writeBody(e.msg, HTTPStatus.badRequest);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void getAllUsersHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        auto users = getAllUsersService();
        res.writeJsonBody(users);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void getUserHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["id"].to!long;
        auto user = getUserService(id);
        res.writeJsonBody(user);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void updateUserHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["id"].to!long;
        auto updateReq = req.json.deserializeJson!UpdateUserRequest;
        auto user = updateUserService(id, updateReq);
        res.writeJsonBody(user);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (ServiceException e) {
        res.writeBody(e.msg, HTTPStatus.badRequest);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void deleteUserHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["id"].to!long;
        deleteUserService(id);
        res.writeVoidBody();
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

// Tasks

void createTaskHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long userId = req.params["id"].to!long;
        auto createReq = req.json.deserializeJson!CreateTaskRequest;
        auto task = createTaskService(userId, createReq);
        res.writeJsonBody(task, HTTPStatus.created);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (ServiceException e) {
        res.writeBody(e.msg, HTTPStatus.badRequest);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void getTasksByUserHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long userId = req.params["id"].to!long;
        auto tasks = getTasksByUserService(userId);
        res.writeJsonBody(tasks);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void getTaskHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["tid"].to!long;
        auto task = getTaskService(id);
        res.writeJsonBody(task);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void updateTaskHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["tid"].to!long;
        auto updateReq = req.json.deserializeJson!UpdateTaskRequest;
        auto task = updateTaskService(id, updateReq);
        res.writeJsonBody(task);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void markTaskDoneHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["tid"].to!long;
        auto task = markTaskDoneService(id);
        res.writeJsonBody(task);
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}

void deleteTaskHandler(HTTPServerRequest req, HTTPServerResponse res) {
    try {
        long id = req.params["tid"].to!long;
        deleteTaskService(id);
        res.writeVoidBody();
    } catch (NotFoundException e) {
        res.writeBody(e.msg, HTTPStatus.notFound);
    } catch (Exception e) {
        res.writeBody(e.msg, HTTPStatus.internalServerError);
    }
}
