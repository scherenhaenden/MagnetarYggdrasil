with AUnit.Reporter.Text;
with AUnit.Run;
with Magnetar.Tests;

procedure Magnetar_Test_Main is
   procedure Run is new AUnit.Run.Test_Runner (Magnetar.Tests.Test_Case);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Magnetar_Test_Main;
