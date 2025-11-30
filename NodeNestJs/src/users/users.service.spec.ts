import { Test, TestingModule } from '@nestjs/testing';
import { UsersService } from './users.service';
import { getRepositoryToken } from '@nestjs/typeorm';
import { User } from './user.entity';
import { Repository } from 'typeorm';
import { NotFoundException } from '@nestjs/common';

const mockUserRepository = {
  create: jest.fn(),
  save: jest.fn(),
  find: jest.fn(),
  findOneBy: jest.fn(),
  merge: jest.fn(),
  delete: jest.fn(),
};

describe('UsersService', () => {
  let service: UsersService;
  let repository: Repository<User>;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        UsersService,
        {
          provide: getRepositoryToken(User),
          useValue: mockUserRepository,
        },
      ],
    }).compile();

    service = module.get<UsersService>(UsersService);
    repository = module.get<Repository<User>>(getRepositoryToken(User));
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('create', () => {
    it('should create a user', async () => {
      const createUserDto = { name: 'Test', email: 'test@example.com' };
      const user = { id: 1, ...createUserDto, tasks: [] };
      mockUserRepository.create.mockReturnValue(user);
      mockUserRepository.save.mockResolvedValue(user);

      expect(await service.create(createUserDto)).toEqual(user);
    });
  });

  describe('findAll', () => {
    it('should return an array of users', async () => {
        const users = [{ id: 1, name: 'Test', email: 'test@example.com' }];
        mockUserRepository.find.mockResolvedValue(users);
        expect(await service.findAll()).toEqual(users);
    });
  });

  describe('findOne', () => {
      it('should return a user if found', async () => {
          const user = { id: 1, name: 'Test', email: 'test@example.com' };
          mockUserRepository.findOneBy.mockResolvedValue(user);
          expect(await service.findOne(1)).toEqual(user);
      });

      it('should throw NotFoundException if not found', async () => {
          mockUserRepository.findOneBy.mockResolvedValue(null);
          await expect(service.findOne(1)).rejects.toThrow(NotFoundException);
      });
  });

  describe('update', () => {
      it('should update a user', async () => {
          const user = { id: 1, name: 'Test', email: 'test@example.com' };
          const updateUserDto = { name: 'Updated' };
          mockUserRepository.findOneBy.mockResolvedValue(user);
          mockUserRepository.save.mockResolvedValue({ ...user, ...updateUserDto });

          expect(await service.update(1, updateUserDto)).toEqual({ ...user, ...updateUserDto });
      });
  });

  describe('remove', () => {
      it('should delete a user', async () => {
          mockUserRepository.delete.mockResolvedValue({ affected: 1 });
          await service.remove(1);
          expect(mockUserRepository.delete).toHaveBeenCalledWith(1);
      });

      it('should throw NotFoundException if user not found', async () => {
          mockUserRepository.delete.mockResolvedValue({ affected: 0 });
          await expect(service.remove(1)).rejects.toThrow(NotFoundException);
      });
  });
});
