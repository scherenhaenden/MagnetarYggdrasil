from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.ext.asyncio import AsyncSession
from app.database import get_db
from app.schemas import TaskCreate, TaskResponse, TaskUpdate
from app.services.task_service import TaskService
from app.repositories.task_repository import TaskRepository
from app.repositories.user_repository import UserRepository

router = APIRouter(tags=["tasks"])

def get_task_service(db: AsyncSession = Depends(get_db)) -> TaskService:
    return TaskService(TaskRepository(db), UserRepository(db))

@router.post("/users/{user_id}/tasks", response_model=TaskResponse, status_code=status.HTTP_201_CREATED)
async def create_task(user_id: int, task: TaskCreate, service: TaskService = Depends(get_task_service)):
    created_task = await service.create_task(user_id, task.title, task.description)
    if not created_task:
        raise HTTPException(status_code=404, detail="User not found")
    return created_task

@router.get("/users/{user_id}/tasks", response_model=list[TaskResponse])
async def get_tasks(user_id: int, service: TaskService = Depends(get_task_service)):
    tasks = await service.get_tasks_by_user_id(user_id)
    if tasks is None:
        raise HTTPException(status_code=404, detail="User not found")
    return tasks

@router.get("/tasks/{task_id}", response_model=TaskResponse)
async def get_task(task_id: int, service: TaskService = Depends(get_task_service)):
    task = await service.get_task_by_id(task_id)
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")
    return task

@router.put("/tasks/{task_id}", response_model=TaskResponse)
async def update_task(task_id: int, task_update: TaskUpdate, service: TaskService = Depends(get_task_service)):
    updated_task = await service.update_task(task_id, task_update.title, task_update.description)
    if not updated_task:
        raise HTTPException(status_code=404, detail="Task not found")
    return updated_task

@router.patch("/tasks/{task_id}/done", response_model=TaskResponse)
async def mark_task_as_done(task_id: int, service: TaskService = Depends(get_task_service)):
    task = await service.mark_task_as_done(task_id)
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")
    return task

@router.delete("/tasks/{task_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_task(task_id: int, service: TaskService = Depends(get_task_service)):
    success = await service.delete_task(task_id)
    if not success:
        raise HTTPException(status_code=404, detail="Task not found")
    return None
