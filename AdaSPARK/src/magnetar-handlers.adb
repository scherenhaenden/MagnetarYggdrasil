with Magnetar.DB;
with Magnetar.Types; use Magnetar.Types;
with Ada.Strings.Fixed;

package body Magnetar.Handlers is

   procedure Handle_Request (Path : String; Method : String; Body : String; Response : out String) is
      Content : String := "HTTP/1.1 200 OK" & ASCII.CR & ASCII.LF & "Content-Type: application/json" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF & "{""status"":""ok""}";
      User_Created_Content : String := "HTTP/1.1 201 Created" & ASCII.CR & ASCII.LF & "Content-Type: application/json" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF & "{""id"":1}";
      Get_Users_Content : String := "HTTP/1.1 200 OK" & ASCII.CR & ASCII.LF & "Content-Type: application/json" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF & "[]";
      Not_Found_Content : String := "HTTP/1.1 404 Not Found" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;
   begin
      if Path = "/health" then
         Ada.Strings.Fixed.Move (Source => Content, Target => Response);
      elsif Path = "/users" and Method = "POST" then
         -- Parse body, create user
         declare
             U : User := Magnetar.DB.Create_User ("New User", "email@example.com");
         begin
             Ada.Strings.Fixed.Move (Source => User_Created_Content, Target => Response);
         end;
      elsif Path = "/users" and Method = "GET" then
         Ada.Strings.Fixed.Move (Source => Get_Users_Content, Target => Response);
      else
         Ada.Strings.Fixed.Move (Source => Not_Found_Content, Target => Response);
      end if;
   end Handle_Request;

end Magnetar.Handlers;
