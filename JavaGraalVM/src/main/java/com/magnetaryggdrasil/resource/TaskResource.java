package com.magnetaryggdrasil.resource;

import com.magnetaryggdrasil.model.Task;
import com.magnetaryggdrasil.service.TaskService;
import jakarta.inject.Inject;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import java.util.List;

@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class TaskResource {

    @Inject
    TaskService taskService;

    @GET
    @Path("users/{id}/tasks")
    public Response getTasksByUserId(@PathParam("id") Long id) {
        List<Task> tasks = taskService.getTasksByUserId(id);
        if (tasks == null) {
             return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.ok(tasks).build();
    }

    @POST
    @Path("users/{id}/tasks")
    public Response createTask(@PathParam("id") Long id, Task task) {
        Task createdTask = taskService.createTask(id, task);
        if (createdTask == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.status(Response.Status.CREATED).entity(createdTask).build();
    }

    @GET
    @Path("tasks/{tid}")
    public Response getTaskById(@PathParam("tid") Long tid) {
        Task task = taskService.getTaskById(tid);
        if (task == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.ok(task).build();
    }

    @PUT
    @Path("tasks/{tid}")
    public Response updateTask(@PathParam("tid") Long tid, Task task) {
        Task updatedTask = taskService.updateTask(tid, task);
        if (updatedTask == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.ok(updatedTask).build();
    }

    @PATCH
    @Path("tasks/{tid}/done")
    public Response markTaskAsDone(@PathParam("tid") Long tid) {
        Task updatedTask = taskService.markTaskAsDone(tid);
        if (updatedTask == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.ok(updatedTask).build();
    }

    @DELETE
    @Path("tasks/{tid}")
    public Response deleteTask(@PathParam("tid") Long tid) {
        boolean deleted = taskService.deleteTask(tid);
        if (!deleted) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.noContent().build();
    }
}
