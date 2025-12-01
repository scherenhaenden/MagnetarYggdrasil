class TasksController < ApplicationController
  def create
    task = TaskService.create(params[:user_id], task_params)
    if task
      render json: task, status: :created
    else
      render json: { error: "Invalid input" }, status: :bad_request
    end
  end

  def index
    tasks = TaskService.list_by_user(params[:user_id])
    render json: tasks
  end

  def show
    task = TaskService.get(params[:id])
    render json: task
  end

  def update
    task = TaskService.update(params[:id], task_params)
    render json: task
  end

  def done
    task = TaskService.mark_done(params[:id])
    render json: task
  end

  def destroy
    TaskService.delete(params[:id])
    head :no_content
  end

  private

  def task_params
    params.require(:task).permit(:title, :description, :done)
  rescue ActionController::ParameterMissing
     params.permit(:title, :description, :done)
  end
end
