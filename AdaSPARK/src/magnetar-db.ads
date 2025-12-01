with Magnetar.Types; use Magnetar.Types;

package Magnetar.DB is

   procedure Init_DB;

   -- Users
   function Create_User (Name : String; Email : String) return User;
   function Get_User (ID : Long_Integer) return User;
   function Get_All_Users return User_Vectors.Vector;
   procedure Update_User (U : User);
   procedure Delete_User (ID : Long_Integer);

   -- Tasks
   function Create_Task (User_ID : Long_Integer; Title : String; Description : String) return Task;
   function Get_Task (ID : Long_Integer) return Task;
   function Get_Tasks_By_User (User_ID : Long_Integer) return Task_Vectors.Vector;
   procedure Update_Task (T : Task);
   procedure Delete_Task (ID : Long_Integer);

end Magnetar.DB;
