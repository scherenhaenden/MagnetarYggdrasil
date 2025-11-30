package com.magnetaryggdrasil.repository;

import com.magnetaryggdrasil.model.Task;
import io.quarkus.hibernate.orm.panache.PanacheRepository;
import jakarta.enterprise.context.ApplicationScoped;
import java.util.List;

@ApplicationScoped
public class TaskRepository implements PanacheRepository<Task> {
    public List<Task> findByUserId(Long userId) {
        return find("user.id", userId).list();
    }
}
