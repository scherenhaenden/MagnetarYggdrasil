import pytest
from httpx import AsyncClient

@pytest.mark.asyncio
async def test_create_user(client: AsyncClient):
    response = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    assert response.status_code == 201
    data = response.json()
    assert data["name"] == "Test User"
    assert data["email"] == "test@example.com"
    assert "id" in data

@pytest.mark.asyncio
async def test_create_user_duplicate_email(client: AsyncClient):
    await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    response = await client.post("/users", json={"name": "Other User", "email": "test@example.com"})
    assert response.status_code == 409

@pytest.mark.asyncio
async def test_get_users(client: AsyncClient):
    await client.post("/users", json={"name": "User 1", "email": "u1@example.com"})
    await client.post("/users", json={"name": "User 2", "email": "u2@example.com"})
    response = await client.get("/users")
    assert response.status_code == 200
    assert len(response.json()) == 2

@pytest.mark.asyncio
async def test_get_user_by_id(client: AsyncClient):
    create_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = create_res.json()["id"]
    response = await client.get(f"/users/{user_id}")
    assert response.status_code == 200
    assert response.json()["id"] == user_id

@pytest.mark.asyncio
async def test_get_user_not_found(client: AsyncClient):
    response = await client.get("/users/999")
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_update_user(client: AsyncClient):
    create_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = create_res.json()["id"]
    response = await client.put(f"/users/{user_id}", json={"name": "Updated Name"})
    assert response.status_code == 200
    assert response.json()["name"] == "Updated Name"
    assert response.json()["email"] == "test@example.com"

@pytest.mark.asyncio
async def test_update_user_duplicate_email(client: AsyncClient):
    await client.post("/users", json={"name": "User 1", "email": "u1@example.com"})
    create_res = await client.post("/users", json={"name": "User 2", "email": "u2@example.com"})
    user_id = create_res.json()["id"]
    response = await client.put(f"/users/{user_id}", json={"email": "u1@example.com"})
    assert response.status_code == 409

@pytest.mark.asyncio
async def test_update_user_not_found(client: AsyncClient):
    response = await client.put("/users/999", json={"name": "Updated Name"})
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_delete_user(client: AsyncClient):
    create_res = await client.post("/users", json={"name": "Test User", "email": "test@example.com"})
    user_id = create_res.json()["id"]
    response = await client.delete(f"/users/{user_id}")
    assert response.status_code == 204
    get_res = await client.get(f"/users/{user_id}")
    assert get_res.status_code == 404

@pytest.mark.asyncio
async def test_delete_user_not_found(client: AsyncClient):
    response = await client.delete("/users/999")
    assert response.status_code == 404
