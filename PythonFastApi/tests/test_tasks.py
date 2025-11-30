import pytest
from httpx import AsyncClient

@pytest.mark.asyncio
async def test_create_task(client: AsyncClient):
    user_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = user_res.json()["id"]
    response = await client.post(f"/users/{user_id}/tasks", json={"title": "Task 1", "description": "Desc"})
    assert response.status_code == 201
    data = response.json()
    assert data["title"] == "Task 1"
    assert data["user_id"] == user_id
    assert data["is_done"] is False

@pytest.mark.asyncio
async def test_create_task_user_not_found(client: AsyncClient):
    response = await client.post("/users/999/tasks", json={"title": "Task 1", "description": "Desc"})
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_get_tasks(client: AsyncClient):
    user_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = user_res.json()["id"]
    await client.post(f"/users/{user_id}/tasks", json={"title": "Task 1", "description": "Desc"})
    await client.post(f"/users/{user_id}/tasks", json={"title": "Task 2", "description": "Desc"})
    response = await client.get(f"/users/{user_id}/tasks")
    assert response.status_code == 200
    assert len(response.json()) == 2

@pytest.mark.asyncio
async def test_get_tasks_user_not_found(client: AsyncClient):
    response = await client.get("/users/999/tasks")
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_get_task_by_id(client: AsyncClient):
    user_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = user_res.json()["id"]
    task_res = await client.post(f"/users/{user_id}/tasks", json={"title": "Task 1", "description": "Desc"})
    task_id = task_res.json()["id"]
    response = await client.get(f"/tasks/{task_id}")
    assert response.status_code == 200
    assert response.json()["id"] == task_id

@pytest.mark.asyncio
async def test_get_task_not_found(client: AsyncClient):
    response = await client.get("/tasks/999")
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_update_task(client: AsyncClient):
    user_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = user_res.json()["id"]
    task_res = await client.post(f"/users/{user_id}/tasks", json={"title": "Task 1", "description": "Desc"})
    task_id = task_res.json()["id"]
    response = await client.put(f"/tasks/{task_id}", json={"title": "Updated Task"})
    assert response.status_code == 200
    assert response.json()["title"] == "Updated Task"

@pytest.mark.asyncio
async def test_update_task_not_found(client: AsyncClient):
    response = await client.put("/tasks/999", json={"title": "Updated Task"})
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_mark_task_done(client: AsyncClient):
    user_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = user_res.json()["id"]
    task_res = await client.post(f"/users/{user_id}/tasks", json={"title": "Task 1", "description": "Desc"})
    task_id = task_res.json()["id"]
    response = await client.patch(f"/tasks/{task_id}/done")
    assert response.status_code == 200
    assert response.json()["is_done"] is True

@pytest.mark.asyncio
async def test_mark_task_done_not_found(client: AsyncClient):
    response = await client.patch("/tasks/999/done")
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_delete_task(client: AsyncClient):
    user_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = user_res.json()["id"]
    task_res = await client.post(f"/users/{user_id}/tasks", json={"title": "Task 1", "description": "Desc"})
    task_id = task_res.json()["id"]
    response = await client.delete(f"/tasks/{task_id}")
    assert response.status_code == 204
    get_res = await client.get(f"/tasks/{task_id}")
    assert get_res.status_code == 404

@pytest.mark.asyncio
async def test_delete_task_not_found(client: AsyncClient):
    response = await client.delete("/tasks/999")
    assert response.status_code == 404
