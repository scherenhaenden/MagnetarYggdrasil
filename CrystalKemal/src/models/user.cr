require "json"

module App
  class User
    include JSON::Serializable

    property id : Int64?
    property username : String
    property email : String

    def initialize(@username : String, @email : String, @id : Int64? = nil)
    end
  end
end
