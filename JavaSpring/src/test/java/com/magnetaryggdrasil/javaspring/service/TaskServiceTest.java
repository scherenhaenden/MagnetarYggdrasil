package com.magnetaryggdrasil.javaspring.service;

import com.magnetaryggdrasil.javaspring.model.Task;
import com.magnetaryggdrasil.javaspring.model.User;
import com.magnetaryggdrasil.javaspring.repository.TaskRepository;
import com.magnetaryggdrasil.javaspring.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class TaskServiceTest {

    @Mock
    private TaskRepository taskRepository;

    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private TaskService taskService;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void createTask_ShouldAssociateWithUserAndSave() {
        User user = User.builder().id(1L).build();
        Task task = Task.builder().title("Task").description("Desc").build();

        when(userRepository.findById(1L)).thenReturn(Optional.of(user));
        when(taskRepository.save(any(Task.class))).thenAnswer(invocation -> invocation.getArgument(0));

        Task created = taskService.createTask(1L, task);

        assertNotNull(created);
        assertEquals(user, created.getUser());
        verify(taskRepository).save(task);
    }

    @Test
    public void createTask_WhenUserNotFound_ShouldThrowException() {
        Task task = Task.builder().title("Task").build();
        when(userRepository.findById(1L)).thenReturn(Optional.empty());

        assertThrows(IllegalArgumentException.class, () -> taskService.createTask(1L, task));
    }
}
