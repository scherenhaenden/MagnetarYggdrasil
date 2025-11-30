package com.magnetaryggdrasil.javaspring.service;

import com.magnetaryggdrasil.javaspring.model.User;
import com.magnetaryggdrasil.javaspring.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private UserService userService;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void createUser_ShouldSaveUser() {
        User user = User.builder().name("John").email("john@example.com").build();
        when(userRepository.existsByEmail("john@example.com")).thenReturn(false);
        when(userRepository.save(any(User.class))).thenReturn(user);

        User created = userService.createUser(user);

        assertNotNull(created);
        assertEquals("John", created.getName());
        verify(userRepository, times(1)).save(user);
    }

    @Test
    public void createUser_WhenEmailExists_ShouldThrowException() {
        User user = User.builder().name("John").email("john@example.com").build();
        when(userRepository.existsByEmail("john@example.com")).thenReturn(true);

        assertThrows(IllegalArgumentException.class, () -> userService.createUser(user));
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void updateUser_ShouldUpdateFields() {
        User existingUser = User.builder().id(1L).name("John").email("john@example.com").build();
        User updates = User.builder().name("Johnny").build();

        when(userRepository.findById(1L)).thenReturn(Optional.of(existingUser));
        when(userRepository.save(any(User.class))).thenAnswer(invocation -> invocation.getArgument(0));

        User updated = userService.updateUser(1L, updates);

        assertEquals("Johnny", updated.getName());
        assertEquals("john@example.com", updated.getEmail()); // Should remain unchanged
    }
}
