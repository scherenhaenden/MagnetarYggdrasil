<?php

namespace App\Controller;

use App\Service\UserService;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class UserController extends AbstractController
{
    public function __construct(private UserService $userService)
    {
    }

    #[Route('/users', name: 'create_user', methods: ['POST'])]
    public function create(Request $request): JsonResponse
    {
        $data = json_decode($request->getContent(), true);

        if (!isset($data['username']) || !isset($data['email'])) {
            return $this->json(['error' => 'Missing required fields'], Response::HTTP_BAD_REQUEST);
        }

        try {
            $user = $this->userService->createUser($data['username'], $data['email']);
            return $this->json($user->toArray(), Response::HTTP_CREATED);
        } catch (\InvalidArgumentException $e) {
            return $this->json(['error' => $e->getMessage()], Response::HTTP_BAD_REQUEST);
        }
    }

    #[Route('/users', name: 'get_users', methods: ['GET'])]
    public function list(): JsonResponse
    {
        $users = $this->userService->getAllUsers();
        $data = array_map(fn($user) => $user->toArray(), $users);
        return $this->json($data);
    }

    #[Route('/users/{id}', name: 'get_user', methods: ['GET'])]
    public function get(int $id): JsonResponse
    {
        $user = $this->userService->getUser($id);
        if (!$user) {
            return $this->json(['error' => 'User not found'], Response::HTTP_NOT_FOUND);
        }
        return $this->json($user->toArray());
    }

    #[Route('/users/{id}', name: 'update_user', methods: ['PUT'])]
    public function update(int $id, Request $request): JsonResponse
    {
        $data = json_decode($request->getContent(), true);
        if (!isset($data['username'])) {
             // Spec says Partial updates allowed but input is { "username": "string" }
             // If username is missing in PUT, it might be weird.
             // "Input: JSON { "username": "string" }"
             return $this->json(['error' => 'Username is required'], Response::HTTP_BAD_REQUEST);
        }

        try {
            $user = $this->userService->updateUser($id, $data['username']);
            if (!$user) {
                return $this->json(['error' => 'User not found'], Response::HTTP_NOT_FOUND);
            }
            return $this->json($user->toArray());
        } catch (\InvalidArgumentException $e) {
             return $this->json(['error' => $e->getMessage()], Response::HTTP_BAD_REQUEST);
        }
    }

    #[Route('/users/{id}', name: 'delete_user', methods: ['DELETE'])]
    public function delete(int $id): Response
    {
        $deleted = $this->userService->deleteUser($id);
        if (!$deleted) {
             // Spec doesn't strictly say 404 for DELETE, but usually nice.
             // "Output: HTTP 204 No Content."
             // Idempotency usually allows 204 even if not found, but "User not found" often 404.
             // I'll return 204 regardless to be safe/idempotent or 404 if strictly following REST.
             // Spec implies 204 is the success case. I will stick to 204 always or 404 if not found.
             // Let's go with 204 always for DELETE as per standard practice, OR 404 if truly not there.
             // The requirement says "Output: HTTP 204 No Content". It doesn't mention error.
             // However, `GET /users/{id}` mentions Error 404.
             // I will return 204.
             return new Response(null, Response::HTTP_NO_CONTENT);
        }
        return new Response(null, Response::HTTP_NO_CONTENT);
    }
}
