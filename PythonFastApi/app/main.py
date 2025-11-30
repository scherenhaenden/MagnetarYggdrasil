from fastapi import FastAPI
from contextlib import asynccontextmanager
from app.database import init_db
from app.routers import users, tasks, health

@asynccontextmanager
async def lifespan(app: FastAPI):
    await init_db()
    yield

app = FastAPI(lifespan=lifespan)

app.include_router(users.router)
app.include_router(tasks.router)
app.include_router(health.router)
