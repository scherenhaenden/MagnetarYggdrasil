#!/bin/bash
set -e

# Start the server in background
echo "Starting server..."
v run src/main.v &
PID=$!
sleep 2

echo "Running tests..."

# Create User
echo "Creating User..."
curl -s -X POST http://localhost:8080/users -H "Content-Type: application/json" -d '{"username": "jules", "email": "jules@example.com"}' > user.json
cat user.json
echo ""
USER_ID=$(jq -r '.id' user.json)

# Create Task
echo "Creating Task for User $USER_ID..."
curl -s -X POST http://localhost:8080/users/$USER_ID/tasks -H "Content-Type: application/json" -d '{"title": "Coding", "description": "Write V code"}' > task.json
cat task.json
echo ""
TASK_ID=$(jq -r '.id' task.json)

# Get User
echo "Getting User $USER_ID..."
curl -s http://localhost:8080/users/$USER_ID | jq .

# Get Tasks
echo "Getting Tasks for User $USER_ID..."
curl -s http://localhost:8080/users/$USER_ID/tasks | jq .

# Update Task
echo "Updating Task $TASK_ID..."
curl -s -X PUT http://localhost:8080/tasks/$TASK_ID -H "Content-Type: application/json" -d '{"title": "Coding V", "description": "Write awesome V code"}' | jq .

# Mark Task Done
echo "Marking Task $TASK_ID done..."
curl -s -X PATCH http://localhost:8080/tasks/$TASK_ID/done | jq .

# Delete Task
echo "Deleting Task $TASK_ID..."
curl -s -X DELETE http://localhost:8080/tasks/$TASK_ID

# Delete User
echo "Deleting User $USER_ID..."
curl -s -X DELETE http://localhost:8080/users/$USER_ID

# Health Check
echo "Health Check..."
curl -s http://localhost:8080/health | jq .

# Cleanup
kill $PID
rm user.json task.json magnetar.db
echo "Tests Passed!"
