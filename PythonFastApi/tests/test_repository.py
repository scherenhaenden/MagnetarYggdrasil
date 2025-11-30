import pytest
from app.repositories.user_repository import UserRepository
from app.repositories.task_repository import TaskRepository
from app.services.user_service import UserService
from app.services.task_service import TaskService
from sqlalchemy.exc import IntegrityError

@pytest.mark.asyncio
async def test_user_repository_create(db_session):
    repo = UserRepository(db_session)
    user = await repo.create_user("Test User", "test@example.com")
    assert user.id is not None
    assert user.name == "Test User"
    assert user.email == "test@example.com"

@pytest.mark.asyncio
async def test_user_repository_create_duplicate_email(db_session):
    repo = UserRepository(db_session)
    await repo.create_user("Test User", "test@example.com")
    user = await repo.create_user("Another User", "test@example.com")
    assert user is None

@pytest.mark.asyncio
async def test_user_repository_get_by_id(db_session):
    repo = UserRepository(db_session)
    created_user = await repo.create_user("Test User", "test@example.com")
    user = await repo.get_user_by_id(created_user.id)
    assert user is not None
    assert user.id == created_user.id

@pytest.mark.asyncio
async def test_user_repository_get_all(db_session):
    repo = UserRepository(db_session)
    await repo.create_user("User 1", "user1@example.com")
    await repo.create_user("User 2", "user2@example.com")
    users = await repo.get_all_users()
    assert len(users) == 2

@pytest.mark.asyncio
async def test_user_repository_update(db_session):
    repo = UserRepository(db_session)
    user = await repo.create_user("Test User", "test@example.com")
    updated_user = await repo.update_user(user, "Updated Name", "updated@example.com")
    assert updated_user.name == "Updated Name"
    assert updated_user.email == "updated@example.com"

@pytest.mark.asyncio
async def test_user_repository_delete(db_session):
    repo = UserRepository(db_session)
    user = await repo.create_user("Test User", "test@example.com")
    await repo.delete_user(user)
    fetched_user = await repo.get_user_by_id(user.id)
    assert fetched_user is None

@pytest.mark.asyncio
async def test_task_repository_create(db_session):
    user_repo = UserRepository(db_session)
    user = await user_repo.create_user("Test User", "test@example.com")
    task_repo = TaskRepository(db_session)
    task = await task_repo.create_task(user.id, "Test Task", "Description")
    assert task.id is not None
    assert task.title == "Test Task"
    assert task.user_id == user.id

@pytest.mark.asyncio
async def test_task_repository_get_by_user(db_session):
    user_repo = UserRepository(db_session)
    user = await user_repo.create_user("Test User", "test@example.com")
    task_repo = TaskRepository(db_session)
    await task_repo.create_task(user.id, "Task 1", "Desc 1")
    await task_repo.create_task(user.id, "Task 2", "Desc 2")
    tasks = await task_repo.get_tasks_by_user_id(user.id)
    assert len(tasks) == 2

@pytest.mark.asyncio
async def test_task_repository_mark_done(db_session):
    user_repo = UserRepository(db_session)
    user = await user_repo.create_user("Test User", "test@example.com")
    task_repo = TaskRepository(db_session)
    task = await task_repo.create_task(user.id, "Test Task", "Description")
    updated_task = await task_repo.mark_task_as_done(task)
    assert updated_task.is_done is True

@pytest.mark.asyncio
async def test_user_service_create_duplicate(db_session):
    repo = UserRepository(db_session)
    service = UserService(repo)
    await service.create_user("U1", "u1@e.com")
    res = await service.create_user("U2", "u1@e.com")
    assert res is None

@pytest.mark.asyncio
async def test_user_service_update_duplicate_email(db_session):
    repo = UserRepository(db_session)
    service = UserService(repo)
    u1 = await service.create_user("U1", "u1@e.com")
    u2 = await service.create_user("U2", "u2@e.com")

    res = await service.update_user(u1.id, None, "u2@e.com")
    assert res is None

@pytest.mark.asyncio
async def test_user_service_update_not_found(db_session):
    repo = UserRepository(db_session)
    service = UserService(repo)
    res = await service.update_user(999, "Name", None)
    assert res is None

@pytest.mark.asyncio
async def test_user_service_delete_not_found(db_session):
    repo = UserRepository(db_session)
    service = UserService(repo)
    res = await service.delete_user(999)
    assert res is False

@pytest.mark.asyncio
async def test_task_service_create_user_not_found(db_session):
    task_repo = TaskRepository(db_session)
    user_repo = UserRepository(db_session)
    service = TaskService(task_repo, user_repo)
    res = await service.create_task(999, "T", "D")
    assert res is None
