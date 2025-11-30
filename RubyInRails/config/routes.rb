Rails.application.routes.draw do
  get '/health', to: 'system#health'

  resources :users, only: [:create, :index, :show, :update, :destroy] do
    resources :tasks, only: [:create, :index], controller: 'tasks'
  end

  resources :tasks, only: [:show, :update, :destroy] do
    member do
      patch :done
    end
  end
end
