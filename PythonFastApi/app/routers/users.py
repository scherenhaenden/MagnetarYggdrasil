from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.ext.asyncio import AsyncSession
from app.database import get_db
from app.schemas import UserCreate, UserResponse, UserUpdate
from app.services.user_service import UserService
from app.repositories.user_repository import UserRepository

router = APIRouter(prefix="/users", tags=["users"])

def get_user_service(db: AsyncSession = Depends(get_db)) -> UserService:
    return UserService(UserRepository(db))

@router.post("", response_model=UserResponse, status_code=status.HTTP_201_CREATED)
async def create_user(user: UserCreate, service: UserService = Depends(get_user_service)):
    created_user = await service.create_user(user.name, user.email)
    if not created_user:
        raise HTTPException(status_code=409, detail="Email already registered")
    return created_user

@router.get("", response_model=list[UserResponse])
async def get_users(service: UserService = Depends(get_user_service)):
    return await service.get_all_users()

@router.get("/{user_id}", response_model=UserResponse)
async def get_user(user_id: int, service: UserService = Depends(get_user_service)):
    user = await service.get_user_by_id(user_id)
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    return user

@router.put("/{user_id}", response_model=UserResponse)
async def update_user(user_id: int, user_update: UserUpdate, service: UserService = Depends(get_user_service)):
    updated_user = await service.update_user(user_id, user_update.name, user_update.email)
    if not updated_user:
        # Check if user exists first to give correct 404 or 409
        if not await service.get_user_by_id(user_id):
             raise HTTPException(status_code=404, detail="User not found")
        raise HTTPException(status_code=409, detail="Email already registered")
    return updated_user

@router.delete("/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_user(user_id: int, service: UserService = Depends(get_user_service)):
    success = await service.delete_user(user_id)
    if not success:
        raise HTTPException(status_code=404, detail="User not found")
    return None
