<?php

namespace App\Tests\Unit;

use App\Entity\User;
use App\Repository\UserRepository;
use App\Service\UserService;
use Doctrine\DBAL\Exception\UniqueConstraintViolationException;
use PHPUnit\Framework\TestCase;

class UserServiceTest extends TestCase
{
    public function testCreateUser()
    {
        $userRepository = $this->createMock(UserRepository::class);
        $userRepository->expects($this->once())
            ->method('save')
            ->with($this->isInstanceOf(User::class), true);

        $service = new UserService($userRepository);
        $user = $service->createUser('testuser', 'test@example.com');

        $this->assertEquals('testuser', $user->getUsername());
        $this->assertEquals('test@example.com', $user->getEmail());
    }

    public function testCreateUserDuplicate()
    {
        $userRepository = $this->createMock(UserRepository::class);
        $userRepository->method('save')
            ->willThrowException($this->createMock(UniqueConstraintViolationException::class));

        $service = new UserService($userRepository);

        $this->expectException(\InvalidArgumentException::class);
        $service->createUser('existing', 'existing@example.com');
    }

    public function testGetUser()
    {
        $user = new User();
        $user->setUsername('test');

        $userRepository = $this->createMock(UserRepository::class);
        $userRepository->expects($this->once())
            ->method('find')
            ->with(1)
            ->willReturn($user);

        $service = new UserService($userRepository);
        $result = $service->getUser(1);

        $this->assertSame($user, $result);
    }
}
