package com.magnetaryggdrasil.javaspring.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.magnetaryggdrasil.javaspring.model.Task;
import com.magnetaryggdrasil.javaspring.model.User;
import com.magnetaryggdrasil.javaspring.service.TaskService;
import com.magnetaryggdrasil.javaspring.service.UserService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(UserController.class)
public class UserControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private UserService userService;

    @MockBean
    private TaskService taskService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    public void createUser_ShouldReturnCreatedUser() throws Exception {
        User user = User.builder().name("John Doe").email("john@example.com").build();
        User createdUser = User.builder().id(1L).name("John Doe").email("john@example.com").build();

        when(userService.createUser(any(User.class))).thenReturn(createdUser);

        mockMvc.perform(post("/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(user)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(1L))
                .andExpect(jsonPath("$.name").value("John Doe"))
                .andExpect(jsonPath("$.email").value("john@example.com"));
    }

    @Test
    public void createUser_WhenEmailExists_ShouldReturnBadRequest() throws Exception {
        User user = User.builder().name("John Doe").email("john@example.com").build();
        when(userService.createUser(any(User.class))).thenThrow(new IllegalArgumentException("Email already exists"));

        mockMvc.perform(post("/users")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(user)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void getAllUsers_ShouldReturnListOfUsers() throws Exception {
        User user1 = User.builder().id(1L).name("John Doe").email("john@example.com").build();
        User user2 = User.builder().id(2L).name("Jane Doe").email("jane@example.com").build();

        when(userService.getAllUsers()).thenReturn(Arrays.asList(user1, user2));

        mockMvc.perform(get("/users"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.size()").value(2))
                .andExpect(jsonPath("$[0].name").value("John Doe"))
                .andExpect(jsonPath("$[1].name").value("Jane Doe"));
    }

    @Test
    public void getUserById_WhenUserExists_ShouldReturnUser() throws Exception {
        User user = User.builder().id(1L).name("John Doe").email("john@example.com").build();
        when(userService.getUserById(1L)).thenReturn(Optional.of(user));

        mockMvc.perform(get("/users/1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(1L))
                .andExpect(jsonPath("$.name").value("John Doe"));
    }

    @Test
    public void getUserById_WhenUserDoesNotExist_ShouldReturnNotFound() throws Exception {
        when(userService.getUserById(1L)).thenReturn(Optional.empty());

        mockMvc.perform(get("/users/1"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void updateUser_WhenUserExists_ShouldReturnUpdatedUser() throws Exception {
        User userUpdate = User.builder().name("John Updated").email("john@example.com").build();
        User updatedUser = User.builder().id(1L).name("John Updated").email("john@example.com").build();

        when(userService.updateUser(eq(1L), any(User.class))).thenReturn(updatedUser);

        mockMvc.perform(put("/users/1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userUpdate)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value("John Updated"));
    }

    @Test
    public void updateUser_WhenUserDoesNotExist_ShouldReturnNotFound() throws Exception {
        User userUpdate = User.builder().name("John Updated").build();
        when(userService.updateUser(eq(1L), any(User.class))).thenThrow(new IllegalArgumentException("User not found with id 1"));

        mockMvc.perform(put("/users/1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userUpdate)))
                .andExpect(status().isNotFound());
    }

    @Test
    public void deleteUser_WhenUserExists_ShouldReturnNoContent() throws Exception {
        mockMvc.perform(delete("/users/1"))
                .andExpect(status().isNoContent());
    }

    @Test
    public void deleteUser_WhenUserDoesNotExist_ShouldReturnNotFound() throws Exception {
        doThrow(new IllegalArgumentException("User not found with id 1")).when(userService).deleteUser(1L);

        mockMvc.perform(delete("/users/1"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void createTask_WhenUserExists_ShouldReturnCreatedTask() throws Exception {
        Task task = Task.builder().title("Task 1").description("Desc 1").build();
        Task createdTask = Task.builder().id(1L).title("Task 1").description("Desc 1").isDone(false).build();

        when(taskService.createTask(eq(1L), any(Task.class))).thenReturn(createdTask);

        mockMvc.perform(post("/users/1/tasks")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(task)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.id").value(1L))
                .andExpect(jsonPath("$.title").value("Task 1"));
    }

    @Test
    public void createTask_WhenUserDoesNotExist_ShouldReturnNotFound() throws Exception {
        Task task = Task.builder().title("Task 1").description("Desc 1").build();
        when(taskService.createTask(eq(1L), any(Task.class))).thenThrow(new IllegalArgumentException("User not found with id 1"));

        mockMvc.perform(post("/users/1/tasks")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(task)))
                .andExpect(status().isNotFound());
    }

    @Test
    public void getTasksByUserId_WhenUserExists_ShouldReturnTaskList() throws Exception {
        Task task1 = Task.builder().id(1L).title("Task 1").build();
        Task task2 = Task.builder().id(2L).title("Task 2").build();

        when(taskService.getTasksByUserId(1L)).thenReturn(Arrays.asList(task1, task2));

        mockMvc.perform(get("/users/1/tasks"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.size()").value(2));
    }

     @Test
    public void getTasksByUserId_WhenUserDoesNotExist_ShouldReturnNotFound() throws Exception {
        when(taskService.getTasksByUserId(1L)).thenThrow(new IllegalArgumentException("User not found with id 1"));

        mockMvc.perform(get("/users/1/tasks"))
                .andExpect(status().isNotFound());
    }
}
