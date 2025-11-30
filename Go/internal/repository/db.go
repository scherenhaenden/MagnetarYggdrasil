package repository

import (
	"magnetar-go/internal/model"

	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
)

func InitDB(dataSourceName string) (*gorm.DB, error) {
	db, err := gorm.Open(sqlite.Open(dataSourceName), &gorm.Config{})
	if err != nil {
		return nil, err
	}

	// Enforce Foreign Keys
	db.Exec("PRAGMA foreign_keys = ON")

	err = db.AutoMigrate(&model.User{}, &model.Task{})
	if err != nil {
		return nil, err
	}

	return db, nil
}
