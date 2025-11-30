# Python (FastAPI) Implementation

## Prerequisites
- Python 3.10+
- pip

## Installation
```bash
cd PythonFastApi
pip install -r requirements.txt
```

## Running the Application
```bash
uvicorn app.main:app --host 0.0.0.0 --port 8000 --reload
```

## Running Tests
```bash
pytest --cov=app tests/
```

## Structure
- **app/main.py**: Entry point.
- **app/routers**: HTTP Handlers.
- **app/services**: Business Logic.
- **app/repositories**: Database Access.
- **app/models.py**: SQLAlchemy Models.
- **app/schemas.py**: Pydantic Models (DTOs).
- **app/database.py**: Database configuration.
