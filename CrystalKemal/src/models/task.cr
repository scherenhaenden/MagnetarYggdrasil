require "json"

module App
  class Task
    include JSON::Serializable

    property id : Int64?
    property user_id : Int64
    property title : String
    property description : String
    property done : Bool

    def initialize(@user_id : Int64, @title : String, @description : String, @done : Bool = false, @id : Int64? = nil)
    end
  end
end
