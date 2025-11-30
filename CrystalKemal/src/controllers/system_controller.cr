require "kemal"
require "json"

module App
  class SystemController
    def self.register
      get "/health" do |env|
        {"status" => "ok", "version" => "1.0.0"}.to_json
      end
    end
  end
end
