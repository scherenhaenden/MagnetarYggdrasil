FROM racket/racket:8.12

WORKDIR /app

# Install system dependencies if any
RUN apt-get update && apt-get install -y sqlite3 libsqlite3-dev

# Copy project files
COPY . .

# Install dependencies
RUN raco pkg install --auto --batch

# Expose port
EXPOSE 8080

# Command to run
CMD ["racket", "main.rkt"]
