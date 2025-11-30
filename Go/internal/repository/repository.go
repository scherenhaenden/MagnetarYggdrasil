package repository

import (
	"magnetar-go/internal/model"

	"gorm.io/gorm"
)

type Repository interface {
	// User
	CreateUser(user *model.User) error
	GetUser(id uint) (*model.User, error)
	GetAllUsers() ([]model.User, error)
	UpdateUser(user *model.User) error
	DeleteUser(id uint) error

	// Task
	CreateTask(task *model.Task) error
	GetTask(id uint) (*model.Task, error)
	GetAllTasksByUserID(userID uint) ([]model.Task, error)
	UpdateTask(task *model.Task) error
	DeleteTask(id uint) error
}

type repository struct {
	db *gorm.DB
}

func NewRepository(db *gorm.DB) Repository {
	return &repository{db: db}
}

func (r *repository) CreateUser(user *model.User) error {
	return r.db.Create(user).Error
}

func (r *repository) GetUser(id uint) (*model.User, error) {
	var user model.User
	if err := r.db.First(&user, id).Error; err != nil {
		return nil, err
	}
	return &user, nil
}

func (r *repository) GetAllUsers() ([]model.User, error) {
	var users []model.User
	if err := r.db.Find(&users).Error; err != nil {
		return nil, err
	}
	return users, nil
}

func (r *repository) UpdateUser(user *model.User) error {
	return r.db.Save(user).Error
}

func (r *repository) DeleteUser(id uint) error {
	return r.db.Delete(&model.User{}, id).Error
}

func (r *repository) CreateTask(task *model.Task) error {
	return r.db.Create(task).Error
}

func (r *repository) GetTask(id uint) (*model.Task, error) {
	var task model.Task
	if err := r.db.First(&task, id).Error; err != nil {
		return nil, err
	}
	return &task, nil
}

func (r *repository) GetAllTasksByUserID(userID uint) ([]model.Task, error) {
	var tasks []model.Task
	if err := r.db.Where("user_id = ?", userID).Find(&tasks).Error; err != nil {
		return nil, err
	}
	return tasks, nil
}

func (r *repository) UpdateTask(task *model.Task) error {
	return r.db.Save(task).Error
}

func (r *repository) DeleteTask(id uint) error {
	return r.db.Delete(&model.Task{}, id).Error
}
