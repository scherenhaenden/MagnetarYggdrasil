package com.magnetaryggdrasil.javaspring.service;

import com.magnetaryggdrasil.javaspring.model.Task;
import com.magnetaryggdrasil.javaspring.model.User;
import com.magnetaryggdrasil.javaspring.repository.TaskRepository;
import com.magnetaryggdrasil.javaspring.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class TaskService {

    private final TaskRepository taskRepository;
    private final UserRepository userRepository;

    @Autowired
    public TaskService(TaskRepository taskRepository, UserRepository userRepository) {
        this.taskRepository = taskRepository;
        this.userRepository = userRepository;
    }

    public Task createTask(Long userId, Task task) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id " + userId));
        task.setUser(user);
        return taskRepository.save(task);
    }

    public List<Task> getTasksByUserId(Long userId) {
        if (!userRepository.existsById(userId)) {
            throw new IllegalArgumentException("User not found with id " + userId);
        }
        return taskRepository.findByUserId(userId);
    }

    public Optional<Task> getTaskById(Long id) {
        return taskRepository.findById(id);
    }

    public Task updateTask(Long id, Task taskDetails) {
        return taskRepository.findById(id).map(task -> {
            if (taskDetails.getTitle() != null) {
                task.setTitle(taskDetails.getTitle());
            }
            if (taskDetails.getDescription() != null) {
                task.setDescription(taskDetails.getDescription());
            }
            // isDone is handled by specific endpoint, but let's allow it here too if needed, though requirements say "updates" and also "PATCH /done"
            if (taskDetails.getIsDone() != null) {
                task.setIsDone(taskDetails.getIsDone());
            }
            return taskRepository.save(task);
        }).orElseThrow(() -> new IllegalArgumentException("Task not found with id " + id));
    }

    public Task markTaskAsDone(Long id) {
        return taskRepository.findById(id).map(task -> {
            task.setIsDone(true);
            return taskRepository.save(task);
        }).orElseThrow(() -> new IllegalArgumentException("Task not found with id " + id));
    }

    public void deleteTask(Long id) {
        if (!taskRepository.existsById(id)) {
            throw new IllegalArgumentException("Task not found with id " + id);
        }
        taskRepository.deleteById(id);
    }
}
