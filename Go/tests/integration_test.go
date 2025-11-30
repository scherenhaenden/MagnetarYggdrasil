package tests

import (
	"bytes"
	"encoding/json"
	"fmt"
	"magnetar-go/internal/api"
	"magnetar-go/internal/model"
	"magnetar-go/internal/repository"
	"magnetar-go/internal/service"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gin-gonic/gin"
	"github.com/stretchr/testify/assert"
)

func setupRouter() *gin.Engine {
	db, _ := repository.InitDB("file::memory:?cache=shared")
	repo := repository.NewRepository(db)
	svc := service.NewService(repo)
	handler := api.NewHandler(svc)
	return api.SetupRouter(handler)
}

func TestIntegration_FullFlow(t *testing.T) {
	router := setupRouter()

	// 1. Create User
	userPayload := map[string]string{"name": "Integration User", "email": "int@example.com"}
	body, _ := json.Marshal(userPayload)
	req, _ := http.NewRequest("POST", "/users", bytes.NewBuffer(body))
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	assert.Equal(t, http.StatusCreated, w.Code)
	var createdUser model.User
	json.Unmarshal(w.Body.Bytes(), &createdUser)
	assert.Equal(t, userPayload["name"], createdUser.Name)
	assert.NotZero(t, createdUser.ID)

	userID := fmt.Sprintf("%d", createdUser.ID)

	// 2. Get User
	req, _ = http.NewRequest("GET", "/users/"+userID, nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)

	// 3. Update User
	updatePayload := map[string]string{"name": "Updated Integration User"}
	body, _ = json.Marshal(updatePayload)
	req, _ = http.NewRequest("PUT", "/users/"+userID, bytes.NewBuffer(body))
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)

	var updatedUser model.User
	json.Unmarshal(w.Body.Bytes(), &updatedUser)
	assert.Equal(t, "Updated Integration User", updatedUser.Name)

	// 4. Create Task
	taskPayload := map[string]string{"title": "My Task", "description": "Do it"}
	body, _ = json.Marshal(taskPayload)
	req, _ = http.NewRequest("POST", "/users/"+userID+"/tasks", bytes.NewBuffer(body))
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusCreated, w.Code)

	var createdTask model.Task
	json.Unmarshal(w.Body.Bytes(), &createdTask)
	assert.Equal(t, "My Task", createdTask.Title)
	taskID := fmt.Sprintf("%d", createdTask.ID)

	// 5. Get Tasks for User
	req, _ = http.NewRequest("GET", "/users/"+userID+"/tasks", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)
	var tasks []model.Task
	json.Unmarshal(w.Body.Bytes(), &tasks)
	assert.Len(t, tasks, 1)

	// 6. Mark Task Done
	req, _ = http.NewRequest("PATCH", "/tasks/"+taskID+"/done", nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)

	var doneTask model.Task
	json.Unmarshal(w.Body.Bytes(), &doneTask)
	assert.True(t, doneTask.Done)

	// 7. Delete Task
	req, _ = http.NewRequest("DELETE", "/tasks/"+taskID, nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)

	// 8. Delete User
	req, _ = http.NewRequest("DELETE", "/users/"+userID, nil)
	w = httptest.NewRecorder()
	router.ServeHTTP(w, req)
	assert.Equal(t, http.StatusOK, w.Code)
}
