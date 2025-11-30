from app.repositories.user_repository import UserRepository
from app.models import User

class UserService:
    def __init__(self, user_repository: UserRepository):
        self.user_repository = user_repository

    async def create_user(self, name: str, email: str) -> User:
        existing_user = await self.user_repository.get_user_by_email(email)
        if existing_user:
            return None # Email already taken
        return await self.user_repository.create_user(name, email)

    async def get_all_users(self) -> list[User]:
        return await self.user_repository.get_all_users()

    async def get_user_by_id(self, user_id: int) -> User | None:
        return await self.user_repository.get_user_by_id(user_id)

    async def update_user(self, user_id: int, name: str | None, email: str | None) -> User | None:
        user = await self.user_repository.get_user_by_id(user_id)
        if not user:
            return None

        if email:
             existing_user = await self.user_repository.get_user_by_email(email)
             if existing_user and existing_user.id != user_id:
                 return None # Email taken by another user

        return await self.user_repository.update_user(user, name, email)

    async def delete_user(self, user_id: int) -> bool:
        user = await self.user_repository.get_user_by_id(user_id)
        if not user:
            return False
        await self.user_repository.delete_user(user)
        return True
