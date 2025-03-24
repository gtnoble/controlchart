with Ada.Calendar;
with Ada_Sqlite3;
with Chart_Types; use Chart_Types;

package Chart_DB is

   type Chart_Database is tagged private;
   
   Database_Error : exception renames Database_Types.Database_Error;

   procedure Open
     (Self : in out Chart_Database;
      Name : String);
      
   procedure Save_Observation
     (Self : in out Chart_Database;
      Value : Float;
      Data_Name : String;
      Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      Annotations : String := "");
      
   procedure Initialize_Chart
     (Self : in out Chart_Database;
      Chart_Name : String;
      Data_Name : String;
      Chart_Type : Chart_Types.Chart_Type;
      Aggregation_Interval : Integer);
      
   procedure Add_Chart_Setup
     (Self : in out Chart_Database;
      Chart_Name : String;
      Setup_Start_Time : Ada.Calendar.Time;
      Setup_End_Time : Ada.Calendar.Time);
      
   procedure Set_Transformation
     (Self : in out Chart_Database;
      Chart_Name : String;
      Transform : Transformation_Type);
      
   function Get_Chart_Data
     (Self : Chart_Database;
      Chart_Name : String;
      Start_Time : Ada.Calendar.Time;
      End_Time : Ada.Calendar.Time) return Chart_Data_Type;
      
private
   type Chart_Database is tagged record
      DB : Ada_Sqlite3.Database;
      Statements : Database_Types.Prepared_Statements;
   end record;

end Chart_DB;
