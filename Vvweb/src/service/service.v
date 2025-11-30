module service

import model
import repository

pub struct Service {
pub mut:
	repo repository.Repository
}

pub fn new_service(repo repository.Repository) Service {
	return Service{repo: repo}
}

pub fn (mut s Service) create_user(req model.CreateUserRequest) !model.User {
	if req.username == '' || req.email == '' {
		return error('Username and email are required')
	}
	user := model.User{
		username: req.username
		email: req.email
	}
	return s.repo.create_user(user)
}

pub fn (mut s Service) get_all_users() ![]model.User {
	return s.repo.get_all_users()
}

pub fn (mut s Service) get_user(id int) !model.User {
	return s.repo.get_user(id)
}

pub fn (mut s Service) update_user(id int, req model.UpdateUserRequest) !model.User {
	if req.username == '' {
		return error('Username is required')
	}
	return s.repo.update_user(id, req.username)
}

pub fn (mut s Service) delete_user(id int) ! {
	s.repo.delete_user(id)!
}

pub fn (mut s Service) create_task(user_id int, req model.CreateTaskRequest) !model.Task {
	if req.title == '' || req.description == '' {
		return error('Title and description are required')
	}
	task := model.Task{
		user_id: user_id
		title: req.title
		description: req.description
		done: false
	}
	return s.repo.create_task(task)
}

pub fn (mut s Service) get_tasks_by_user(user_id int) ![]model.Task {
	return s.repo.get_tasks_by_user(user_id)
}

pub fn (mut s Service) get_task(id int) !model.Task {
	return s.repo.get_task(id)
}

pub fn (mut s Service) update_task(id int, req model.UpdateTaskRequest) !model.Task {
	if req.title == '' || req.description == '' {
		return error('Title and description are required')
	}
	return s.repo.update_task(id, req.title, req.description)
}

pub fn (mut s Service) set_task_done(id int, done bool) !model.Task {
	return s.repo.set_task_done(id, done)
}

pub fn (mut s Service) delete_task(id int) ! {
	s.repo.delete_task(id)!
}
