package api

import (
	"github.com/gin-gonic/gin"
)

func SetupRouter(h *Handler) *gin.Engine {
	r := gin.Default()

	r.GET("/health", h.Health)

	// Users
	r.POST("/users", h.CreateUser)
	r.GET("/users", h.GetAllUsers)
	r.GET("/users/:id", h.GetUser)
	r.PUT("/users/:id", h.UpdateUser)
	r.DELETE("/users/:id", h.DeleteUser)

	// Tasks
	r.POST("/users/:id/tasks", h.CreateTask)
	r.GET("/users/:id/tasks", h.GetAllTasks)
	r.GET("/tasks/:tid", h.GetTask)
	r.PUT("/tasks/:tid", h.UpdateTask)
	r.PATCH("/tasks/:tid/done", h.MarkTaskDone)
	r.DELETE("/tasks/:tid", h.DeleteTask)

	return r
}
