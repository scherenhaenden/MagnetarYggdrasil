package main

import (
	"log"
	"magnetar-go/internal/api"
	"magnetar-go/internal/repository"
	"magnetar-go/internal/service"
)

func main() {
	db, err := repository.InitDB("magnetar.db")
	if err != nil {
		log.Fatal("Failed to connect to database:", err)
	}

	repo := repository.NewRepository(db)
	svc := service.NewService(repo)
	handler := api.NewHandler(svc)
	router := api.SetupRouter(handler)

	log.Println("Server starting on :8080")
	if err := router.Run(":8080"); err != nil {
		log.Fatal("Failed to run server:", err)
	}
}
