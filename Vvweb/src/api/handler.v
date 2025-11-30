module api

import vweb
import json
import service
import model

pub struct App {
	vweb.Context
pub mut:
	svc service.Service
}

pub fn new_app(svc service.Service) App {
	return App{
		svc: svc
	}
}

// System
['/health']
pub fn (mut app App) health() vweb.Result {
	return app.json({
		'status': 'ok'
		'version': '1.0.0'
	})
}

// Users
['/users'; post]
pub fn (mut app App) create_user() vweb.Result {
	req := json.decode(model.CreateUserRequest, app.req.data) or {
		app.set_status(400, 'Bad Request')
		return app.json({'error': 'Invalid JSON'})
	}
	user := app.svc.create_user(req) or {
		app.set_status(400, 'Bad Request')
		return app.json({'error': err.msg()})
	}
	app.set_status(201, 'Created')
	return app.json(user)
}

['/users']
pub fn (mut app App) get_users() vweb.Result {
	users := app.svc.get_all_users() or {
		app.set_status(500, 'Internal Server Error')
		return app.json({'error': err.msg()})
	}
	return app.json(users)
}

['/users/:id']
pub fn (mut app App) get_user(id int) vweb.Result {
	user := app.svc.get_user(id) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': 'User not found'})
	}
	return app.json(user)
}

['/users/:id'; put]
pub fn (mut app App) update_user(id int) vweb.Result {
	req := json.decode(model.UpdateUserRequest, app.req.data) or {
		app.set_status(400, 'Bad Request')
		return app.json({'error': 'Invalid JSON'})
	}
	user := app.svc.update_user(id, req) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': err.msg()})
	}
	return app.json(user)
}

['/users/:id'; delete]
pub fn (mut app App) delete_user(id int) vweb.Result {
	app.svc.delete_user(id) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': 'User not found'})
	}
	app.set_status(204, 'No Content')
	return app.text('')
}

// Tasks
['/users/:user_id/tasks'; post]
pub fn (mut app App) create_task(user_id int) vweb.Result {
	req := json.decode(model.CreateTaskRequest, app.req.data) or {
		app.set_status(400, 'Bad Request')
		return app.json({'error': 'Invalid JSON'})
	}
	task := app.svc.create_task(user_id, req) or {
		app.set_status(400, 'Bad Request')
		return app.json({'error': err.msg()})
	}
	app.set_status(201, 'Created')
	return app.json(task)
}

['/users/:user_id/tasks']
pub fn (mut app App) get_tasks_by_user(user_id int) vweb.Result {
	tasks := app.svc.get_tasks_by_user(user_id) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': err.msg()})
	}
	return app.json(tasks)
}

['/tasks/:id']
pub fn (mut app App) get_task(id int) vweb.Result {
	task := app.svc.get_task(id) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': 'Task not found'})
	}
	return app.json(task)
}

['/tasks/:id'; put]
pub fn (mut app App) update_task(id int) vweb.Result {
	req := json.decode(model.UpdateTaskRequest, app.req.data) or {
		app.set_status(400, 'Bad Request')
		return app.json({'error': 'Invalid JSON'})
	}
	task := app.svc.update_task(id, req) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': err.msg()})
	}
	return app.json(task)
}

['/tasks/:id/done'; patch]
pub fn (mut app App) set_task_done(id int) vweb.Result {
    mut done := true
    if app.req.data.len > 0 {
         req := json.decode(model.TaskDoneRequest, app.req.data) or {
             model.TaskDoneRequest{done: true}
         }
         done = req.done
    }

	task := app.svc.set_task_done(id, done) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': err.msg()})
	}
	return app.json(task)
}

['/tasks/:id'; delete]
pub fn (mut app App) delete_task(id int) vweb.Result {
	app.svc.delete_task(id) or {
		app.set_status(404, 'Not Found')
		return app.json({'error': 'Task not found'})
	}
	app.set_status(204, 'No Content')
	return app.text('')
}
