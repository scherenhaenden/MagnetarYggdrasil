<?php

namespace App\Service;

use App\Entity\User;
use App\Repository\UserRepository;
use Doctrine\DBAL\Exception\UniqueConstraintViolationException;

class UserService
{
    public function __construct(private UserRepository $userRepository)
    {
    }

    public function createUser(string $username, string $email): User
    {
        // Simple validation, can be enhanced
        if (empty($username) || empty($email)) {
             throw new \InvalidArgumentException('Username and email are required.');
        }

        $user = new User();
        $user->setUsername($username);
        $user->setEmail($email);

        try {
            $this->userRepository->save($user, true);
        } catch (UniqueConstraintViolationException $e) {
             throw new \InvalidArgumentException('User with this username or email already exists.');
        }

        return $user;
    }

    public function getUser(int $id): ?User
    {
        return $this->userRepository->find($id);
    }

    public function getAllUsers(): array
    {
        return $this->userRepository->findAll();
    }

    public function updateUser(int $id, string $username): ?User
    {
        $user = $this->userRepository->find($id);
        if (!$user) {
            return null;
        }

        if (empty($username)) {
             throw new \InvalidArgumentException('Username is required.');
        }

        $user->setUsername($username);

        try {
            $this->userRepository->save($user, true);
        } catch (UniqueConstraintViolationException $e) {
             throw new \InvalidArgumentException('Username already exists.');
        }

        return $user;
    }

    public function deleteUser(int $id): bool
    {
        $user = $this->userRepository->find($id);
        if (!$user) {
            return false;
        }

        $this->userRepository->remove($user, true);
        return true;
    }
}
