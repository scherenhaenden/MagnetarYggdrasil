from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from app.models import Task

class TaskRepository:
    def __init__(self, db: AsyncSession):
        self.db = db

    async def create_task(self, user_id: int, title: str, description: str) -> Task:
        task = Task(title=title, description=description, user_id=user_id)
        self.db.add(task)
        await self.db.commit()
        await self.db.refresh(task)
        return task

    async def get_tasks_by_user_id(self, user_id: int) -> list[Task]:
        result = await self.db.execute(select(Task).where(Task.user_id == user_id))
        return result.scalars().all()

    async def get_task_by_id(self, task_id: int) -> Task | None:
        result = await self.db.execute(select(Task).where(Task.id == task_id))
        return result.scalar_one_or_none()

    async def update_task(self, task: Task, title: str | None, description: str | None) -> Task:
        if title:
            task.title = title
        if description:
            task.description = description
        await self.db.commit()
        await self.db.refresh(task)
        return task

    async def mark_task_as_done(self, task: Task) -> Task:
        task.is_done = True
        await self.db.commit()
        await self.db.refresh(task)
        return task

    async def delete_task(self, task: Task) -> None:
        await self.db.delete(task)
        await self.db.commit()
