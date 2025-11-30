from app.repositories.task_repository import TaskRepository
from app.repositories.user_repository import UserRepository
from app.models import Task

class TaskService:
    def __init__(self, task_repository: TaskRepository, user_repository: UserRepository):
        self.task_repository = task_repository
        self.user_repository = user_repository

    async def create_task(self, user_id: int, title: str, description: str) -> Task | None:
        user = await self.user_repository.get_user_by_id(user_id)
        if not user:
            return None
        return await self.task_repository.create_task(user_id, title, description)

    async def get_tasks_by_user_id(self, user_id: int) -> list[Task] | None:
        user = await self.user_repository.get_user_by_id(user_id)
        if not user:
             return None
        return await self.task_repository.get_tasks_by_user_id(user_id)

    async def get_task_by_id(self, task_id: int) -> Task | None:
        return await self.task_repository.get_task_by_id(task_id)

    async def update_task(self, task_id: int, title: str | None, description: str | None) -> Task | None:
        task = await self.task_repository.get_task_by_id(task_id)
        if not task:
            return None
        return await self.task_repository.update_task(task, title, description)

    async def mark_task_as_done(self, task_id: int) -> Task | None:
        task = await self.task_repository.get_task_by_id(task_id)
        if not task:
            return None
        return await self.task_repository.mark_task_as_done(task)

    async def delete_task(self, task_id: int) -> bool:
        task = await self.task_repository.get_task_by_id(task_id)
        if not task:
            return False
        await self.task_repository.delete_task(task)
        return True
