require "spec"
require "kemal"
require "../src/app"

# Setup DB for tests
App::Database.setup

module App
  class Database
    # Re-opening class to add helper for testing
    def self.clean
      # We use the pool directly to exec cleanups
      pool.exec "DELETE FROM tasks"
      pool.exec "DELETE FROM users"
      pool.exec "DELETE FROM sqlite_sequence WHERE name='tasks'"
      pool.exec "DELETE FROM sqlite_sequence WHERE name='users'"
    end
  end
end

Spec.before_each do
  App::Database.clean
end
