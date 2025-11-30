<?php

namespace App\Tests\Unit;

use App\Entity\Task;
use App\Entity\User;
use App\Repository\TaskRepository;
use App\Repository\UserRepository;
use App\Service\TaskService;
use PHPUnit\Framework\TestCase;

class TaskServiceTest extends TestCase
{
    public function testCreateTask()
    {
        $user = new User();
        $userRepository = $this->createMock(UserRepository::class);
        $userRepository->method('find')->willReturn($user);

        $taskRepository = $this->createMock(TaskRepository::class);
        $taskRepository->expects($this->once())
            ->method('save')
            ->with($this->isInstanceOf(Task::class), true);

        $service = new TaskService($taskRepository, $userRepository);
        $task = $service->createTask(1, 'Task 1', 'Desc');

        $this->assertEquals('Task 1', $task->getTitle());
        $this->assertEquals($user, $task->getUser());
    }

    public function testMarkTaskDone()
    {
        $task = new Task();
        $taskRepository = $this->createMock(TaskRepository::class);
        $taskRepository->method('find')->willReturn($task);
        $taskRepository->expects($this->once())->method('save');

        $userRepository = $this->createMock(UserRepository::class);

        $service = new TaskService($taskRepository, $userRepository);
        $updatedTask = $service->markTaskDone(1, true);

        $this->assertTrue($updatedTask->isDone());
    }
}
