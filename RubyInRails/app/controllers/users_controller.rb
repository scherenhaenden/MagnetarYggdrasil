class UsersController < ApplicationController
  def create
    user = UserService.create(user_params)
    if user
      render json: user, status: :created
    else
      render json: { error: "Invalid input" }, status: :bad_request
    end
  rescue ActiveRecord::RecordNotUnique
    render json: { error: "User already exists" }, status: :bad_request
  end

  def index
    users = UserService.list_all
    render json: users
  end

  def show
    user = UserService.get(params[:id])
    render json: user
  end

  def update
    user = UserService.update(params[:id], user_params)
    render json: user
  end

  def destroy
    UserService.delete(params[:id])
    head :no_content
  end

  private

  def user_params
    params.require(:user).permit(:username, :email)
  rescue ActionController::ParameterMissing
    # Handle cases where json root is missing or raw json is sent without root
    params.permit(:username, :email)
  end
end
