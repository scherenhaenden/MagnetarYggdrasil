# Java (GraalVM Native Image) Implementation

This is the Java implementation using **Quarkus** compiled to a Native Image with GraalVM.

## 🛠 Prerequisites

*   Java 21+
*   Maven 3.9+
*   GraalVM (optional, for native build)

## 🚀 Running the Application (JVM Mode)

To run the application in JVM mode (development/standard):

```bash
mvn quarkus:dev
```

Or package and run:

```bash
mvn package
java -jar target/quarkus-app/quarkus-run.jar
```

## ⚡ Running the Application (Native Mode)

To build a native executable (requires GraalVM installed and configured):

```bash
mvn package -Pnative
./target/magnetar-yggdrasil-java-graalvm-runner
```

## 🧪 Running Tests

```bash
mvn test
```

## 🏗 Architecture

*   **Framework**: Quarkus
*   **Database**: SQLite (via JDBC)
*   **ORM**: Hibernate ORM with Panache
*   **JSON**: Jackson (via RESTEasy Reactive)

## 📂 Structure

*   `src/main/java/com/magnetaryggdrasil/resource`: REST Controllers
*   `src/main/java/com/magnetaryggdrasil/service`: Business Logic
*   `src/main/java/com/magnetaryggdrasil/repository`: Data Access
*   `src/main/java/com/magnetaryggdrasil/model`: JPA Entities
