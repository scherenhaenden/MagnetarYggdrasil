package service

import (
	"errors"
	"magnetar-go/internal/model"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type MockRepository struct {
	mock.Mock
}

func (m *MockRepository) CreateUser(user *model.User) error {
	args := m.Called(user)
	return args.Error(0)
}

func (m *MockRepository) GetUser(id uint) (*model.User, error) {
	args := m.Called(id)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*model.User), args.Error(1)
}

func (m *MockRepository) GetAllUsers() ([]model.User, error) {
	args := m.Called()
	return args.Get(0).([]model.User), args.Error(1)
}

func (m *MockRepository) UpdateUser(user *model.User) error {
	args := m.Called(user)
	return args.Error(0)
}

func (m *MockRepository) DeleteUser(id uint) error {
	args := m.Called(id)
	return args.Error(0)
}

func (m *MockRepository) CreateTask(task *model.Task) error {
	args := m.Called(task)
	return args.Error(0)
}

func (m *MockRepository) GetTask(id uint) (*model.Task, error) {
	args := m.Called(id)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*model.Task), args.Error(1)
}

func (m *MockRepository) GetAllTasksByUserID(userID uint) ([]model.Task, error) {
	args := m.Called(userID)
	return args.Get(0).([]model.Task), args.Error(1)
}

func (m *MockRepository) UpdateTask(task *model.Task) error {
	args := m.Called(task)
	return args.Error(0)
}

func (m *MockRepository) DeleteTask(id uint) error {
	args := m.Called(id)
	return args.Error(0)
}

func TestCreateUser(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	user := &model.User{Name: "John", Email: "john@example.com"}

	mockRepo.On("CreateUser", user).Return(nil)

	err := svc.CreateUser(user)
	assert.NoError(t, err)
	mockRepo.AssertExpectations(t)
}

func TestGetUser(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	user := &model.User{ID: 1, Name: "John"}

	mockRepo.On("GetUser", uint(1)).Return(user, nil)

	result, err := svc.GetUser(1)
	assert.NoError(t, err)
	assert.Equal(t, user, result)
	mockRepo.AssertExpectations(t)
}

func TestUpdateUser(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	user := &model.User{ID: 1, Name: "John", Email: "old@example.com"}
	mockRepo.On("GetUser", uint(1)).Return(user, nil)
	mockRepo.On("UpdateUser", mock.AnythingOfType("*model.User")).Return(nil)

	updates := map[string]interface{}{
		"name": "Jane",
	}

	updatedUser, err := svc.UpdateUser(1, updates)
	assert.NoError(t, err)
	assert.Equal(t, "Jane", updatedUser.Name)
	assert.Equal(t, "old@example.com", updatedUser.Email)
}

func TestDeleteUser(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	user := &model.User{ID: 1}
	mockRepo.On("GetUser", uint(1)).Return(user, nil)
	mockRepo.On("DeleteUser", uint(1)).Return(nil)

	err := svc.DeleteUser(1)
	assert.NoError(t, err)
}

func TestCreateTask(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	user := &model.User{ID: 1}
	task := &model.Task{Title: "Task 1"}

	mockRepo.On("GetUser", uint(1)).Return(user, nil)
	mockRepo.On("CreateTask", task).Return(nil)

	err := svc.CreateTask(1, task)
	assert.NoError(t, err)
	assert.Equal(t, uint(1), task.UserID)
}

func TestCreateTask_UserNotFound(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	task := &model.Task{Title: "Task 1"}

	mockRepo.On("GetUser", uint(1)).Return(nil, errors.New("not found"))

	err := svc.CreateTask(1, task)
	assert.Error(t, err)
	assert.Equal(t, ErrUserNotFound, err)
}

func TestGetTask(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	task := &model.Task{ID: 1, Title: "Task 1"}
	mockRepo.On("GetTask", uint(1)).Return(task, nil)

	result, err := svc.GetTask(1)
	assert.NoError(t, err)
	assert.Equal(t, task, result)
}

func TestGetAllTasksByUserID(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	user := &model.User{ID: 1}
	tasks := []model.Task{{ID: 1, Title: "Task 1"}}

	mockRepo.On("GetUser", uint(1)).Return(user, nil)
	mockRepo.On("GetAllTasksByUserID", uint(1)).Return(tasks, nil)

	result, err := svc.GetAllTasksByUserID(1)
	assert.NoError(t, err)
	assert.Equal(t, tasks, result)
}

func TestUpdateTask(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	task := &model.Task{ID: 1, Title: "Old Title"}
	mockRepo.On("GetTask", uint(1)).Return(task, nil)
	mockRepo.On("UpdateTask", mock.AnythingOfType("*model.Task")).Return(nil)

	updates := map[string]interface{}{
		"title": "New Title",
	}

	updatedTask, err := svc.UpdateTask(1, updates)
	assert.NoError(t, err)
	assert.Equal(t, "New Title", updatedTask.Title)
}

func TestMarkTaskDone(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	task := &model.Task{ID: 1, Done: false}
	mockRepo.On("GetTask", uint(1)).Return(task, nil)
	mockRepo.On("UpdateTask", mock.AnythingOfType("*model.Task")).Return(nil)

	updatedTask, err := svc.MarkTaskDone(1)
	assert.NoError(t, err)
	assert.True(t, updatedTask.Done)
}

func TestDeleteTask(t *testing.T) {
	mockRepo := new(MockRepository)
	svc := NewService(mockRepo)

	task := &model.Task{ID: 1}
	mockRepo.On("GetTask", uint(1)).Return(task, nil)
	mockRepo.On("DeleteTask", uint(1)).Return(nil)

	err := svc.DeleteTask(1)
	assert.NoError(t, err)
}
