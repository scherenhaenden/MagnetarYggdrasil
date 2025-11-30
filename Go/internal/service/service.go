package service

import (
	"errors"
	"magnetar-go/internal/model"
	"magnetar-go/internal/repository"
)

var (
	ErrUserNotFound = errors.New("user not found")
	ErrTaskNotFound = errors.New("task not found")
)

type Service struct {
	repo repository.Repository
}

func NewService(repo repository.Repository) *Service {
	return &Service{repo: repo}
}

func (s *Service) CreateUser(user *model.User) error {
	return s.repo.CreateUser(user)
}

func (s *Service) GetUser(id uint) (*model.User, error) {
	return s.repo.GetUser(id)
}

func (s *Service) GetAllUsers() ([]model.User, error) {
	return s.repo.GetAllUsers()
}

func (s *Service) UpdateUser(id uint, updates map[string]interface{}) (*model.User, error) {
	user, err := s.repo.GetUser(id)
	if err != nil {
		return nil, err
	}

	if name, ok := updates["name"].(string); ok {
		user.Name = name
	}
	if email, ok := updates["email"].(string); ok {
		user.Email = email
	}

	if err := s.repo.UpdateUser(user); err != nil {
		return nil, err
	}
	return user, nil
}

func (s *Service) DeleteUser(id uint) error {
	_, err := s.repo.GetUser(id)
	if err != nil {
		return err
	}
	return s.repo.DeleteUser(id)
}

func (s *Service) CreateTask(userID uint, task *model.Task) error {
	// Verify user exists
	_, err := s.repo.GetUser(userID)
	if err != nil {
		return ErrUserNotFound
	}
	task.UserID = userID
	return s.repo.CreateTask(task)
}

func (s *Service) GetTask(id uint) (*model.Task, error) {
	return s.repo.GetTask(id)
}

func (s *Service) GetAllTasksByUserID(userID uint) ([]model.Task, error) {
	// Verify user exists
	_, err := s.repo.GetUser(userID)
	if err != nil {
		return nil, ErrUserNotFound
	}
	return s.repo.GetAllTasksByUserID(userID)
}

func (s *Service) UpdateTask(id uint, updates map[string]interface{}) (*model.Task, error) {
	task, err := s.repo.GetTask(id)
	if err != nil {
		return nil, err
	}

	if title, ok := updates["title"].(string); ok {
		task.Title = title
	}
	if description, ok := updates["description"].(string); ok {
		task.Description = description
	}

	if err := s.repo.UpdateTask(task); err != nil {
		return nil, err
	}
	return task, nil
}

func (s *Service) MarkTaskDone(id uint) (*model.Task, error) {
	task, err := s.repo.GetTask(id)
	if err != nil {
		return nil, err
	}
	task.Done = true
	if err := s.repo.UpdateTask(task); err != nil {
		return nil, err
	}
	return task, nil
}

func (s *Service) DeleteTask(id uint) error {
	_, err := s.repo.GetTask(id)
	if err != nil {
		return err
	}
	return s.repo.DeleteTask(id)
}
