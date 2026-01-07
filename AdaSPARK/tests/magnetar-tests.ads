with Magnetar.Handlers;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Magnetar.Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);
   procedure Test_Health_Check (T : in out AUnit.Test_Cases.Test_Case'Class);

end Magnetar.Tests;
