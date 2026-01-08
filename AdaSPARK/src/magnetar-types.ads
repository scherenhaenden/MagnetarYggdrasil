with Ada.Containers.Vectors;

package Magnetar.Types is

   type User is record
      ID        : Long_Integer := 0;
      Name      : String (1 .. 100) := (others => ' ');
      Email     : String (1 .. 100) := (others => ' ');
      Name_Len  : Natural := 0;
      Email_Len : Natural := 0;
   end record;

   type Task is record
      ID              : Long_Integer := 0;
      User_ID         : Long_Integer := 0;
      Title           : String (1 .. 100) := (others => ' ');
      Description     : String (1 .. 200) := (others => ' ');
      Is_Done         : Boolean := False;
      Title_Len       : Natural := 0;
      Description_Len : Natural := 0;
   end record;

   package User_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => User);

   package Task_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Task);

end Magnetar.Types;
