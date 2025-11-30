package com.magnetaryggdrasil.service;

import com.magnetaryggdrasil.model.User;
import com.magnetaryggdrasil.repository.UserRepository;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import java.util.List;

@ApplicationScoped
public class UserService {

    @Inject
    UserRepository userRepository;

    public List<User> getAllUsers() {
        return userRepository.listAll();
    }

    public User getUserById(Long id) {
        return userRepository.findById(id);
    }

    @Transactional
    public User createUser(User user) {
        if (userRepository.findByEmail(user.email) != null) {
            throw new IllegalArgumentException("Email already exists");
        }
        userRepository.persist(user);
        return user;
    }

    @Transactional
    public User updateUser(Long id, User userDetails) {
        User user = userRepository.findById(id);
        if (user == null) {
            return null;
        }
        if (userDetails.name != null) {
            user.name = userDetails.name;
        }
        if (userDetails.email != null) {
             if (!user.email.equals(userDetails.email) && userRepository.findByEmail(userDetails.email) != null) {
                throw new IllegalArgumentException("Email already exists");
            }
            user.email = userDetails.email;
        }
        return user;
    }

    @Transactional
    public boolean deleteUser(Long id) {
        return userRepository.deleteById(id);
    }
}
