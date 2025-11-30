import { Test, TestingModule } from '@nestjs/testing';
import { TasksService } from './tasks.service';
import { getRepositoryToken } from '@nestjs/typeorm';
import { Task } from './task.entity';
import { User } from '../users/user.entity';
import { Repository } from 'typeorm';
import { NotFoundException } from '@nestjs/common';

const mockTaskRepository = {
  create: jest.fn(),
  save: jest.fn(),
  findOneBy: jest.fn(),
  merge: jest.fn(),
  delete: jest.fn(),
};

const mockUserRepository = {
    findOneBy: jest.fn(),
    findOne: jest.fn(),
};

describe('TasksService', () => {
  let service: TasksService;
  let taskRepository: Repository<Task>;
  let userRepository: Repository<User>;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        TasksService,
        {
          provide: getRepositoryToken(Task),
          useValue: mockTaskRepository,
        },
        {
            provide: getRepositoryToken(User),
            useValue: mockUserRepository,
        }
      ],
    }).compile();

    service = module.get<TasksService>(TasksService);
    taskRepository = module.get<Repository<Task>>(getRepositoryToken(Task));
    userRepository = module.get<Repository<User>>(getRepositoryToken(User));
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('create', () => {
      it('should create a task', async () => {
          const user = { id: 1, name: 'Test', email: 'test@example.com' };
          const createTaskDto = { title: 'Task', description: 'Desc' };
          const task = { id: 1, ...createTaskDto, user, done: false };

          mockUserRepository.findOneBy.mockResolvedValue(user);
          mockTaskRepository.create.mockReturnValue(task);
          mockTaskRepository.save.mockResolvedValue(task);

          expect(await service.create(1, createTaskDto)).toEqual(task);
      });

      it('should throw NotFoundException if user not found', async () => {
          mockUserRepository.findOneBy.mockResolvedValue(null);
          await expect(service.create(1, { title: 'T', description: 'D' })).rejects.toThrow(NotFoundException);
      });
  });

  describe('findAllByUser', () => {
      it('should return tasks for a user', async () => {
          const user = { id: 1, tasks: [{ id: 1, title: 'Task' }] };
          mockUserRepository.findOne.mockResolvedValue(user);

          expect(await service.findAllByUser(1)).toEqual(user.tasks);
      });
  });
});
