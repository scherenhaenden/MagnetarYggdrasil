package com.magnetaryggdrasil.javaspring.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.magnetaryggdrasil.javaspring.model.Task;
import com.magnetaryggdrasil.javaspring.service.TaskService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(TaskController.class)
public class TaskControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private TaskService taskService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    public void getTaskById_WhenTaskExists_ShouldReturnTask() throws Exception {
        Task task = Task.builder().id(1L).title("Task 1").build();
        when(taskService.getTaskById(1L)).thenReturn(Optional.of(task));

        mockMvc.perform(get("/tasks/1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(1L))
                .andExpect(jsonPath("$.title").value("Task 1"));
    }

    @Test
    public void getTaskById_WhenTaskDoesNotExist_ShouldReturnNotFound() throws Exception {
        when(taskService.getTaskById(1L)).thenReturn(Optional.empty());

        mockMvc.perform(get("/tasks/1"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void updateTask_WhenTaskExists_ShouldReturnUpdatedTask() throws Exception {
        Task taskUpdate = Task.builder().title("Updated Task").build();
        Task updatedTask = Task.builder().id(1L).title("Updated Task").build();

        when(taskService.updateTask(eq(1L), any(Task.class))).thenReturn(updatedTask);

        mockMvc.perform(put("/tasks/1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(taskUpdate)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.title").value("Updated Task"));
    }

    @Test
    public void updateTask_WhenTaskDoesNotExist_ShouldReturnNotFound() throws Exception {
        Task taskUpdate = Task.builder().title("Updated Task").build();
        when(taskService.updateTask(eq(1L), any(Task.class))).thenThrow(new IllegalArgumentException("Task not found with id 1"));

        mockMvc.perform(put("/tasks/1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(taskUpdate)))
                .andExpect(status().isNotFound());
    }

    @Test
    public void markTaskAsDone_WhenTaskExists_ShouldReturnUpdatedTask() throws Exception {
        Task updatedTask = Task.builder().id(1L).title("Task 1").isDone(true).build();
        when(taskService.markTaskAsDone(1L)).thenReturn(updatedTask);

        mockMvc.perform(patch("/tasks/1/done"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.isDone").value(true));
    }

    @Test
    public void markTaskAsDone_WhenTaskDoesNotExist_ShouldReturnNotFound() throws Exception {
        when(taskService.markTaskAsDone(1L)).thenThrow(new IllegalArgumentException("Task not found with id 1"));

        mockMvc.perform(patch("/tasks/1/done"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void deleteTask_WhenTaskExists_ShouldReturnNoContent() throws Exception {
        mockMvc.perform(delete("/tasks/1"))
                .andExpect(status().isNoContent());
    }

    @Test
    public void deleteTask_WhenTaskDoesNotExist_ShouldReturnNotFound() throws Exception {
        doThrow(new IllegalArgumentException("Task not found with id 1")).when(taskService).deleteTask(1L);

        mockMvc.perform(delete("/tasks/1"))
                .andExpect(status().isNotFound());
    }
}
