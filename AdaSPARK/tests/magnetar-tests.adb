with Magnetar.Handlers;
with AUnit.Assertions; use AUnit.Assertions;

package body Magnetar.Tests is

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Health_Check'Access, "Test Health Check");
   end Register_Tests;

   procedure Test_Health_Check (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Response : String (1 .. 1024);
   begin
      Magnetar.Handlers.Handle_Request ("/health", "GET", "", Response);
      Assert (Response (1 .. 15) = "HTTP/1.1 200 OK", "Health check failed");
   end Test_Health_Check;

end Magnetar.Tests;
