package api

import (
	"magnetar-go/internal/model"
	"magnetar-go/internal/service"
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"
)

type Handler struct {
	svc *service.Service
}

func NewHandler(svc *service.Service) *Handler {
	return &Handler{svc: svc}
}

func (h *Handler) Health(c *gin.Context) {
	c.Status(http.StatusOK)
}

// CreateUser handles the creation of a new user from the request context.
func (h *Handler) CreateUser(c *gin.Context) {
	var user model.User
	if err := c.ShouldBindJSON(&user); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}
	if err := h.svc.CreateUser(&user); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusCreated, user)
}

// GetAllUsers retrieves all users and returns them in JSON format.
func (h *Handler) GetAllUsers(c *gin.Context) {
	users, err := h.svc.GetAllUsers()
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, users)
}

func (h *Handler) GetUser(c *gin.Context) {
	id, err := strconv.Atoi(c.Param("id"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid id"})
		return
	}
	user, err := h.svc.GetUser(uint(id))
	if err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "user not found"})
		return
	}
	c.JSON(http.StatusOK, user)
}

// UpdateUser handles the HTTP request to update a user by ID.
func (h *Handler) UpdateUser(c *gin.Context) {
	id, err := strconv.Atoi(c.Param("id"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid id"})
		return
	}
	var updates map[string]interface{}
	if err := c.ShouldBindJSON(&updates); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}
	user, err := h.svc.UpdateUser(uint(id), updates)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, user)
}

// DeleteUser handles the deletion of a user by their ID.
func (h *Handler) DeleteUser(c *gin.Context) {
	id, err := strconv.Atoi(c.Param("id"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid id"})
		return
	}
	if err := h.svc.DeleteUser(uint(id)); err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "user not found"})
		return
	}
	c.Status(http.StatusOK)
}

// Tasks

// CreateTask handles the creation of a new task for a user.
func (h *Handler) CreateTask(c *gin.Context) {
	id, err := strconv.Atoi(c.Param("id"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid user id"})
		return
	}
	var task model.Task
	if err := c.ShouldBindJSON(&task); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}
	if err := h.svc.CreateTask(uint(id), &task); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusCreated, task)
}

// GetAllTasks retrieves all tasks for a specific user identified by the user ID in the request context.
func (h *Handler) GetAllTasks(c *gin.Context) {
	id, err := strconv.Atoi(c.Param("id"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid user id"})
		return
	}
	tasks, err := h.svc.GetAllTasksByUserID(uint(id))
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, tasks)
}

// GetTask retrieves a task by its ID from the context and returns it as JSON.
func (h *Handler) GetTask(c *gin.Context) {
	tid, err := strconv.Atoi(c.Param("tid"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid task id"})
		return
	}
	task, err := h.svc.GetTask(uint(tid))
	if err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "task not found"})
		return
	}
	c.JSON(http.StatusOK, task)
}

// UpdateTask updates a task based on the provided task ID and JSON updates.
func (h *Handler) UpdateTask(c *gin.Context) {
	tid, err := strconv.Atoi(c.Param("tid"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid task id"})
		return
	}
	var updates map[string]interface{}
	if err := c.ShouldBindJSON(&updates); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}
	task, err := h.svc.UpdateTask(uint(tid), updates)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, task)
}

// MarkTaskDone marks a task as done based on the task ID provided in the request context.
func (h *Handler) MarkTaskDone(c *gin.Context) {
	tid, err := strconv.Atoi(c.Param("tid"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid task id"})
		return
	}
	task, err := h.svc.MarkTaskDone(uint(tid))
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, task)
}

func (h *Handler) DeleteTask(c *gin.Context) {
	tid, err := strconv.Atoi(c.Param("tid"))
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "invalid task id"})
		return
	}
	if err := h.svc.DeleteTask(uint(tid)); err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "task not found"})
		return
	}
	c.Status(http.StatusOK)
}
