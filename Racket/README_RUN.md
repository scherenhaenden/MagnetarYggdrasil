# How to Run MagnetarYggdrasil - Racket

## Prerequisites
*   Racket 8.12+
*   SQLite3

## Setup
1.  Install dependencies:
    ```bash
    raco pkg install --auto --batch
    ```

## Running
```bash
racket main.rkt
```

## Docker
```bash
docker build -t magnetar-racket .
docker run -p 8080:8080 magnetar-racket
```
