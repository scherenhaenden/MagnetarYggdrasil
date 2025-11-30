package com.magnetaryggdrasil.javaspring.controller;

import com.magnetaryggdrasil.javaspring.model.Task;
import com.magnetaryggdrasil.javaspring.service.TaskService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/tasks")
public class TaskController {

    private final TaskService taskService;

    @Autowired
    public TaskController(TaskService taskService) {
        this.taskService = taskService;
    }

    @GetMapping("/{tid}")
    /**
     * Retrieves a task by its ID.
     */
    public ResponseEntity<Task> getTaskById(@PathVariable Long tid) {
        return taskService.getTaskById(tid)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @PutMapping("/{tid}")
    /**
     * Updates a task identified by its ID.
     */
    public ResponseEntity<Task> updateTask(@PathVariable Long tid, @RequestBody Task task) {
        try {
            Task updatedTask = taskService.updateTask(tid, task);
            return ResponseEntity.ok(updatedTask);
        } catch (IllegalArgumentException e) {
             return ResponseEntity.notFound().build();
        }
    }

    @PatchMapping("/{tid}/done")
    /**
     * Marks a task as done and returns the updated task.
     */
    public ResponseEntity<Task> markTaskAsDone(@PathVariable Long tid) {
        try {
            Task updatedTask = taskService.markTaskAsDone(tid);
            return ResponseEntity.ok(updatedTask);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.notFound().build();
        }
    }

    @DeleteMapping("/{tid}")
    /**
     * Deletes a task by its ID.
     */
    public ResponseEntity<Void> deleteTask(@PathVariable Long tid) {
        try {
            taskService.deleteTask(tid);
            return ResponseEntity.noContent().build();
        } catch (IllegalArgumentException e) {
            return ResponseEntity.notFound().build();
        }
    }
}
