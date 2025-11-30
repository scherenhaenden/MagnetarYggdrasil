# How to Run PHPSymphony

## Prerequisites
*   PHP 8.1+
*   Composer
*   SQLite3

## Setup

1.  **Install Dependencies:**
    ```bash
    composer install
    ```

2.  **Environment Configuration:**
    Ensure `.env` contains:
    ```
    DATABASE_URL="sqlite:///%kernel.project_dir%/var/data.db"
    ```

3.  **Database Setup:**
    Since the application should auto-create the DB on start (or we use migration), for this setup:
    ```bash
    php bin/console doctrine:database:create
    php bin/console doctrine:schema:create
    ```
    *Note: A real production setup would use Migrations.*

## Running the Server

Start the Symfony local server or PHP built-in server:

```bash
php -S 127.0.0.1:8000 -t public
```

## Running Tests

```bash
php bin/phpunit
```

## API Endpoints

*   `GET /health`
*   `POST /users`
*   `GET /users`
*   `GET /users/{id}`
*   `PUT /users/{id}`
*   `DELETE /users/{id}`
*   `POST /users/{id}/tasks`
*   `GET /users/{id}/tasks`
*   `GET /tasks/{id}`
*   `PUT /tasks/{id}`
*   `PATCH /tasks/{id}/done`
*   `DELETE /tasks/{id}`
