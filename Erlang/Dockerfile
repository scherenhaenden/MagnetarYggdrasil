FROM erlang:24-alpine AS builder

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache git build-base

COPY . .

# Compile and create release
# We need to run rebar3 as prod profile
RUN rebar3 as prod release

FROM alpine:3.19

# Install runtime dependencies if needed (openssl, libstdc++, etc for esqlite/crypto)
RUN apk add --no-cache openssl ncurses-libs libstdc++

WORKDIR /app

# Copy the release from builder
COPY --from=builder /app/_build/prod/rel/magnetar /app/magnetar

# Expose port
EXPOSE 8080

# Run
CMD ["/app/magnetar/bin/magnetar", "foreground"]
