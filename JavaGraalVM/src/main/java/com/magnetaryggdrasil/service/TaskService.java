package com.magnetaryggdrasil.service;

import com.magnetaryggdrasil.model.Task;
import com.magnetaryggdrasil.model.User;
import com.magnetaryggdrasil.repository.TaskRepository;
import com.magnetaryggdrasil.repository.UserRepository;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import java.util.List;

@ApplicationScoped
public class TaskService {

    @Inject
    TaskRepository taskRepository;

    @Inject
    UserRepository userRepository;

    public List<Task> getTasksByUserId(Long userId) {
        if (userRepository.findById(userId) == null) {
            return null; // Or throw exception
        }
        return taskRepository.findByUserId(userId);
    }

    public Task getTaskById(Long id) {
        return taskRepository.findById(id);
    }

    @Transactional
    public Task createTask(Long userId, Task task) {
        User user = userRepository.findById(userId);
        if (user == null) {
            return null;
        }
        task.user = user;
        taskRepository.persist(task);
        return task;
    }

    @Transactional
    public Task updateTask(Long id, Task taskDetails) {
        Task task = taskRepository.findById(id);
        if (task == null) {
            return null;
        }
        if (taskDetails.title != null) {
            task.title = taskDetails.title;
        }
        if (taskDetails.description != null) {
            task.description = taskDetails.description;
        }
        return task;
    }

    @Transactional
    public Task markTaskAsDone(Long id) {
        Task task = taskRepository.findById(id);
        if (task == null) {
            return null;
        }
        task.done = true;
        return task;
    }

    @Transactional
    public boolean deleteTask(Long id) {
        return taskRepository.deleteById(id);
    }
}
