FROM ubuntu:latest

# Install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libsqlite3-dev \
    libssl-dev \
    wget \
    git \
    zlib1g-dev \
    curl \
    sudo

# Install Kore
RUN curl -sSf https://kore.io/install | bash

WORKDIR /app

# Copy project files
COPY . .

# Build
RUN make

# Expose port (8080 is default)
EXPOSE 8080

# Run
CMD ["kore", "run"]
