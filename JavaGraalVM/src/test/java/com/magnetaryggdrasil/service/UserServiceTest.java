package com.magnetaryggdrasil.service;

import com.magnetaryggdrasil.model.User;
import com.magnetaryggdrasil.repository.UserRepository;
import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.junit.mockito.InjectMock;
import jakarta.inject.Inject;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;

@QuarkusTest
public class UserServiceTest {

    @Inject
    UserService userService;

    @InjectMock
    UserRepository userRepository;

    @Test
    public void testCreateUser() {
        User user = new User("Test User", "test@example.com");
        Mockito.when(userRepository.findByEmail("test@example.com")).thenReturn(null);

        userService.createUser(user);

        Mockito.verify(userRepository).persist(user);
    }

    @Test
    public void testCreateUserDuplicateEmail() {
        User user = new User("Test User", "test@example.com");
        Mockito.when(userRepository.findByEmail("test@example.com")).thenReturn(new User());

        assertThrows(IllegalArgumentException.class, () -> userService.createUser(user));
    }
}
