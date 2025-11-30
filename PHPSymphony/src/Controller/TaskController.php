<?php

namespace App\Controller;

use App\Service\TaskService;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class TaskController extends AbstractController
{
    public function __construct(private TaskService $taskService)
    {
    }

    #[Route('/users/{id}/tasks', name: 'create_task', methods: ['POST'])]
    public function create(int $id, Request $request): JsonResponse
    {
        $data = json_decode($request->getContent(), true);

        if (!isset($data['title']) || !isset($data['description'])) {
            return $this->json(['error' => 'Missing required fields'], Response::HTTP_BAD_REQUEST);
        }

        try {
            $task = $this->taskService->createTask($id, $data['title'], $data['description']);
            if (!$task) {
                return $this->json(['error' => 'User not found'], Response::HTTP_NOT_FOUND); // Implicit check
            }
            return $this->json($task->toArray(), Response::HTTP_CREATED);
        } catch (\InvalidArgumentException $e) {
            return $this->json(['error' => $e->getMessage()], Response::HTTP_BAD_REQUEST);
        }
    }

    #[Route('/users/{id}/tasks', name: 'list_user_tasks', methods: ['GET'])]
    public function listForUser(int $id): JsonResponse
    {
        $tasks = $this->taskService->getTasksForUser($id);
        if ($tasks === null) {
             // Implicitly user not found
             return $this->json(['error' => 'User not found'], Response::HTTP_NOT_FOUND);
        }
        $data = array_map(fn($task) => $task->toArray(), $tasks);
        return $this->json($data);
    }

    #[Route('/tasks/{id}', name: 'get_task', methods: ['GET'])]
    public function get(int $id): JsonResponse
    {
        $task = $this->taskService->getTask($id);
        if (!$task) {
            return $this->json(['error' => 'Task not found'], Response::HTTP_NOT_FOUND);
        }
        return $this->json($task->toArray());
    }

    #[Route('/tasks/{id}', name: 'update_task', methods: ['PUT'])]
    public function update(int $id, Request $request): JsonResponse
    {
        $data = json_decode($request->getContent(), true);

        // Spec: Input: JSON { "title": "...", "description": "..." }
        // Does not explicitly say partial, but typically PUT is replace, PATCH is partial.
        // However, requirements for User PUT says "Partial updates allowed".
        // For Task PUT, it lists both title and description in input.
        // I will treat it as update what is provided, defaulting to current if not provided?
        // Or require both?
        // "Input: JSON { "title": "...", "description": "..." }" implies both are expected.
        // But to be safe and robust, I'll allow updating either if provided, or expect both.
        // Let's assume standard PUT: replace resource representation. But I'll implement as 'update fields provided'.

        $title = $data['title'] ?? '';
        $description = $data['description'] ?? '';

        // If strict PUT, we might validate presence.
        // "Input: JSON { "title": "...", "description": "..." }" - I'll assume they are sent.

        $task = $this->taskService->updateTask($id, $title, $description);
        if (!$task) {
            return $this->json(['error' => 'Task not found'], Response::HTTP_NOT_FOUND);
        }
        return $this->json($task->toArray());
    }

    #[Route('/tasks/{id}/done', name: 'mark_task_done', methods: ['PATCH'])]
    public function markDone(int $id, Request $request): JsonResponse
    {
        // Input: None (or JSON { "done": true })
        // Spec says Output: Updated task object with done=true.
        // Implies we just set done=true.

        $task = $this->taskService->markTaskDone($id, true);
        if (!$task) {
            return $this->json(['error' => 'Task not found'], Response::HTTP_NOT_FOUND);
        }
        return $this->json($task->toArray());
    }

    #[Route('/tasks/{id}', name: 'delete_task', methods: ['DELETE'])]
    public function delete(int $id): Response
    {
        $this->taskService->deleteTask($id);
        return new Response(null, Response::HTTP_NO_CONTENT);
    }
}
