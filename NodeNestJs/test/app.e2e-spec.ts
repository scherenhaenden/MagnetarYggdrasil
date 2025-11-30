import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import * as request from 'supertest';
import { AppModule } from './../src/app.module';
import {
  FastifyAdapter,
  NestFastifyApplication,
} from '@nestjs/platform-fastify';

describe('AppController (e2e)', () => {
  let app: NestFastifyApplication;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication<NestFastifyApplication>(
      new FastifyAdapter(),
    );
    app.useGlobalPipes(new ValidationPipe());
    await app.init();
    await app.getHttpAdapter().getInstance().ready();
  });

  afterAll(async () => {
    await app.close();
  });

  it('/health (GET)', () => {
    return request(app.getHttpServer())
      .get('/health')
      .expect(200)
      .expect({ status: 'ok' });
  });

  let userId: number;
  let taskId: number;

  it('Create User', async () => {
    const response = await request(app.getHttpServer())
      .post('/users')
      .send({ name: 'John Doe', email: 'john@example.com' })
      .expect(201);

    userId = response.body.id;
    expect(response.body.name).toBe('John Doe');
    expect(response.body.email).toBe('john@example.com');
  });

  it('Get Users', async () => {
    const response = await request(app.getHttpServer())
        .get('/users')
        .expect(200);
    expect(Array.isArray(response.body)).toBeTruthy();
    expect(response.body.length).toBeGreaterThan(0);
  });

  it('Get User By ID', async () => {
    const response = await request(app.getHttpServer())
        .get(`/users/${userId}`)
        .expect(200);
    expect(response.body.id).toBe(userId);
  });

  it('Update User', async () => {
    const response = await request(app.getHttpServer())
        .put(`/users/${userId}`)
        .send({ name: 'Jane Doe' })
        .expect(200);
    expect(response.body.name).toBe('Jane Doe');
    expect(response.body.email).toBe('john@example.com');
  });

  it('Create Task', async () => {
      const response = await request(app.getHttpServer())
          .post(`/users/${userId}/tasks`)
          .send({ title: 'Test Task', description: 'Test Description' })
          .expect(201);

      taskId = response.body.id;
      expect(response.body.title).toBe('Test Task');
      expect(response.body.user.id).toBe(userId);
  });

  it('Get Tasks for User', async () => {
      const response = await request(app.getHttpServer())
          .get(`/users/${userId}/tasks`)
          .expect(200);
      expect(Array.isArray(response.body)).toBeTruthy();
      expect(response.body.length).toBeGreaterThan(0);
  });

  it('Get Task By ID', async () => {
      const response = await request(app.getHttpServer())
          .get(`/tasks/${taskId}`)
          .expect(200);
      expect(response.body.id).toBe(taskId);
  });

  it('Update Task', async () => {
      const response = await request(app.getHttpServer())
          .put(`/tasks/${taskId}`)
          .send({ title: 'Updated Task' })
          .expect(200);
      expect(response.body.title).toBe('Updated Task');
  });

  it('Mark Task as Done', async () => {
      const response = await request(app.getHttpServer())
          .patch(`/tasks/${taskId}/done`)
          .expect(200);
      expect(response.body.done).toBe(true);
  });

  it('Delete Task', async () => {
      await request(app.getHttpServer())
          .delete(`/tasks/${taskId}`)
          .expect(204);

      await request(app.getHttpServer())
          .get(`/tasks/${taskId}`)
          .expect(404);
  });

  it('Delete User', async () => {
      await request(app.getHttpServer())
          .delete(`/users/${userId}`)
          .expect(204);

      await request(app.getHttpServer())
          .get(`/users/${userId}`)
          .expect(404);
  });
});
