package repository

import (
	"magnetar-go/internal/model"
	"testing"

	"github.com/stretchr/testify/assert"
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
)

func setupTestDB(t *testing.T) Repository {
	db, err := gorm.Open(sqlite.Open("file::memory:?cache=shared"), &gorm.Config{})
	assert.NoError(t, err)

	db.Exec("PRAGMA foreign_keys = ON")
	err = db.AutoMigrate(&model.User{}, &model.Task{})
	assert.NoError(t, err)

	return NewRepository(db)
}

func TestRepository_UserCRUD(t *testing.T) {
	repo := setupTestDB(t)

	user := &model.User{Name: "Test User", Email: "test@example.com"}
	err := repo.CreateUser(user)
	assert.NoError(t, err)
	assert.NotZero(t, user.ID)

	fetchedUser, err := repo.GetUser(user.ID)
	assert.NoError(t, err)
	assert.Equal(t, user.Name, fetchedUser.Name)
	assert.Equal(t, user.Email, fetchedUser.Email)

	user.Name = "Updated Name"
	err = repo.UpdateUser(user)
	assert.NoError(t, err)

	fetchedUser, err = repo.GetUser(user.ID)
	assert.NoError(t, err)
	assert.Equal(t, "Updated Name", fetchedUser.Name)

	err = repo.DeleteUser(user.ID)
	assert.NoError(t, err)

	_, err = repo.GetUser(user.ID)
	assert.Error(t, err)
}

func TestRepository_TaskCRUD(t *testing.T) {
	repo := setupTestDB(t)

	user := &model.User{Name: "Task User", Email: "task@example.com"}
	repo.CreateUser(user)

	task := &model.Task{UserID: user.ID, Title: "Test Task", Description: "Desc"}
	err := repo.CreateTask(task)
	assert.NoError(t, err)
	assert.NotZero(t, task.ID)

	fetchedTask, err := repo.GetTask(task.ID)
	assert.NoError(t, err)
	assert.Equal(t, task.Title, fetchedTask.Title)

	task.Done = true
	err = repo.UpdateTask(task)
	assert.NoError(t, err)

	fetchedTask, err = repo.GetTask(task.ID)
	assert.NoError(t, err)
	assert.True(t, fetchedTask.Done)

	err = repo.DeleteTask(task.ID)
	assert.NoError(t, err)

	_, err = repo.GetTask(task.ID)
	assert.Error(t, err)
}

func TestRepository_ForeignKey(t *testing.T) {
	repo := setupTestDB(t)

	// Try to create task for non-existent user
	task := &model.Task{UserID: 999, Title: "Orphan Task", Description: "Desc"}
	err := repo.CreateTask(task)
	assert.Error(t, err) // Should fail due to FK constraint
}

func TestRepository_UniqueConstraint(t *testing.T) {
	repo := setupTestDB(t)

	user1 := &model.User{Name: "User 1", Email: "unique@example.com"}
	repo.CreateUser(user1)

	user2 := &model.User{Name: "User 2", Email: "unique@example.com"}
	err := repo.CreateUser(user2)
	assert.Error(t, err) // Should fail due to unique email
}
