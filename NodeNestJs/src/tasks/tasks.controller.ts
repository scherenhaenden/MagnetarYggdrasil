import { Controller, Get, Post, Body, Put, Param, Delete, Patch, ParseIntPipe, HttpCode, HttpStatus } from '@nestjs/common';
import { TasksService } from './tasks.service';
import { CreateTaskDto, UpdateTaskDto } from './dto/task.dto';
import { Task } from './task.entity';

@Controller()
export class TasksController {
  constructor(private readonly tasksService: TasksService) {}

  @Post('users/:userId/tasks')
  create(@Param('userId', ParseIntPipe) userId: number, @Body() createTaskDto: CreateTaskDto): Promise<Task> {
    return this.tasksService.create(userId, createTaskDto);
  }

  @Get('users/:userId/tasks')
  findAllByUser(@Param('userId', ParseIntPipe) userId: number): Promise<Task[]> {
    return this.tasksService.findAllByUser(userId);
  }

  @Get('tasks/:id')
  findOne(@Param('id', ParseIntPipe) id: number): Promise<Task> {
    return this.tasksService.findOne(id);
  }

  @Put('tasks/:id')
  update(@Param('id', ParseIntPipe) id: number, @Body() updateTaskDto: UpdateTaskDto): Promise<Task> {
    return this.tasksService.update(id, updateTaskDto);
  }

  @Patch('tasks/:id/done')
  markAsDone(@Param('id', ParseIntPipe) id: number): Promise<Task> {
    return this.tasksService.markAsDone(id);
  }

  @Delete('tasks/:id')
  @HttpCode(HttpStatus.NO_CONTENT)
  remove(@Param('id', ParseIntPipe) id: number): Promise<void> {
    return this.tasksService.remove(id);
  }
}
