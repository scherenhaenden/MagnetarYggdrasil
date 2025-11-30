<?php

namespace App\Service;

use App\Entity\Task;
use App\Repository\TaskRepository;
use App\Repository\UserRepository;

class TaskService
{
    public function __construct(
        private TaskRepository $taskRepository,
        private UserRepository $userRepository
    ) {
    }

    public function createTask(int $userId, string $title, string $description): ?Task
    {
        $user = $this->userRepository->find($userId);
        if (!$user) {
            return null;
        }

        if (empty($title)) {
             throw new \InvalidArgumentException('Title is required.');
        }

        $task = new Task();
        $task->setTitle($title);
        $task->setDescription($description);
        $task->setUser($user);
        $task->setDone(false);

        $this->taskRepository->save($task, true);

        return $task;
    }

    public function getTask(int $id): ?Task
    {
        return $this->taskRepository->find($id);
    }

    public function getTasksForUser(int $userId): ?array
    {
        $user = $this->userRepository->find($userId);
        if (!$user) {
            return null; // Or empty array, but requirement says 404 if user not found for GET /users/{id}
                         // However, GET /users/{id}/tasks spec says "JSON array of tasks for that user (HTTP 200)"
                         // Usually implies 404 if user doesn't exist, but sticking to "return array" if user exists.
                         // Let's check requirement: "Output: JSON array of tasks for that user (HTTP 200)"
                         // If user doesn't exist, typically 404. I'll return null here to signal "User not found".
        }
        return $user->getTasks()->toArray();
    }

    public function updateTask(int $id, string $title, string $description): ?Task
    {
        $task = $this->taskRepository->find($id);
        if (!$task) {
            return null;
        }

        if (!empty($title)) {
            $task->setTitle($title);
        }
        if (!empty($description)) {
            $task->setDescription($description);
        }

        $this->taskRepository->save($task, true);

        return $task;
    }

    public function markTaskDone(int $id, bool $done): ?Task
    {
        $task = $this->taskRepository->find($id);
        if (!$task) {
            return null;
        }

        $task->setDone($done);
        $this->taskRepository->save($task, true);

        return $task;
    }

    public function deleteTask(int $id): bool
    {
        $task = $this->taskRepository->find($id);
        if (!$task) {
            return false;
        }

        $this->taskRepository->remove($task, true);
        return true;
    }
}
