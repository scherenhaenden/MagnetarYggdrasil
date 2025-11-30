package com.magnetaryggdrasil.resource;

import com.magnetaryggdrasil.model.User;
import com.magnetaryggdrasil.repository.UserRepository;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.greaterThan;

@QuarkusTest
public class UserResourceTest {

    @Inject
    UserRepository userRepository;

    @BeforeEach
    public void setup() {
        io.quarkus.narayana.jta.QuarkusTransaction.requiringNew().run(() -> userRepository.deleteAll());
    }

    @Test
    public void testCreateAndGetUser() {
        given()
            .contentType("application/json")
            .body("{\"name\": \"Alice\", \"email\": \"alice@example.com\"}")
            .when().post("/users")
            .then()
            .statusCode(201)
            .body("name", is("Alice"))
            .body("email", is("alice@example.com"))
            .body("id", greaterThan(0));

        given()
            .when().get("/users")
            .then()
            .statusCode(200)
            .body("size()", is(1));
    }
}
