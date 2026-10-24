with Magnetar.DB;
with Magnetar.Handlers;
with Ada.Text_IO;

package body Magnetar.Service is

   procedure Start is
      -- Mock server loop
      Input_Path : String := "/health";
      Response : String (1 .. 1024);
   begin
      Magnetar.DB.Init_DB;
      Ada.Text_IO.Put_Line ("Server started on port 8080");

      -- Simulate one request
      Magnetar.Handlers.Handle_Request(Input_Path, "GET", "", Response);
      Ada.Text_IO.Put_Line ("Response: " & Response);
   end Start;

end Magnetar.Service;
