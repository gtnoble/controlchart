with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Chart_DB.SQL_Statements;
with Chart_DB.Functions;
with Chart_DB.Database_Types; use Chart_DB.Database_Types;

package body Chart_DB is
   
   function To_Unix_Time (Time : Ada.Calendar.Time) return Integer is
   begin
      return Integer(Ada.Calendar.Conversions.To_Unix_Time(Time));
   end To_Unix_Time;
   
   function From_Unix_Time (Unix_Time : Integer) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Conversions.From_Unix_Time(Long_Integer(Unix_Time));
   end From_Unix_Time;
   
   procedure Open
     (Self : in out Chart_Database;
      Name : String)
   is
      use Ada_Sqlite3;
      use SQL_Statements;
   begin
      -- Open database
      Self.DB := Open(Name, OPEN_READWRITE or OPEN_CREATE);
      
      -- Enable foreign keys
      Execute(Self.DB, "PRAGMA foreign_keys = ON");
      
      -- Create tables
      Execute(Self.DB, Create_Chart_Data_Table);
      Execute(Self.DB, Create_Chart_Annotations_Table);
      Execute(Self.DB, Create_Chart_Table);
      Execute(Self.DB, Create_Setup_Table);
      Execute(Self.DB, Create_Transformations_Table);
      
      -- Create indexes
      Execute(Self.DB, Create_Chart_Data_Index);
      Execute(Self.DB, Create_Chart_Index);
      Execute(Self.DB, Create_Setup_Index);
      Execute(Self.DB, Create_Annotations_Index);
      Execute(Self.DB, Create_Transformations_Index);
      
      -- Create views
      Execute(Self.DB, Create_Latest_Setup_View);
      Execute(Self.DB, Create_Base_Data_View);
      Execute(Self.DB, Create_Setup_Stats_View);
      
      -- Register functions
      Functions.Register_Functions(Self.DB);
      
      -- Prepare statements
      Database_Types.Prepare_Statements(Self.DB, Self.Statements);
   end Open;
   
   procedure Save_Observation
     (Self : in out Chart_Database;
      Value : Float;
      Data_Name : String;
      Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      Annotations : String := "")
   is
      use Ada_Sqlite3;
      
      Unix_Time : constant Integer := To_Unix_Time(Time);
   begin
      -- Insert observation using prepared statement
      Bind_Int(Self.Statements(Stmt_Insert_Observation), 1, Unix_Time);
      Bind_Double(Self.Statements(Stmt_Insert_Observation), 2, Long_Float(Value));
      Bind_Text(Self.Statements(Stmt_Insert_Observation), 3, Data_Name);
      Step(Self.Statements(Stmt_Insert_Observation));
      Reset(Self.Statements(Stmt_Insert_Observation));
      
      -- Add annotation if provided
      if Annotations /= "" then
         declare
            Row_ID : constant Integer := Last_Insert_Row_ID(Self.DB);
         begin
            Bind_Int(Self.Statements(Stmt_Insert_Annotation), 1, Row_ID);
            Bind_Text(Self.Statements(Stmt_Insert_Annotation), 2, Annotations);
            Bind_Int(Self.Statements(Stmt_Insert_Annotation), 3, Unix_Time);
            Step(Self.Statements(Stmt_Insert_Annotation));
            Reset(Self.Statements(Stmt_Insert_Annotation));
         end;
      end if;
   end Save_Observation;
   
   procedure Initialize_Chart
     (Self : in out Chart_Database;
      Chart_Name : String;
      Data_Name : String;
      Chart_Type : Chart_Types.Chart_Type;
      Aggregation_Interval : Integer)
   is
      use Ada_Sqlite3;
   begin
      Bind_Text(Self.Statements(Stmt_Initialize_Chart), 1, Chart_Name);
      Bind_Text(Self.Statements(Stmt_Initialize_Chart), 2, To_String(Chart_Type));
      Bind_Text(Self.Statements(Stmt_Initialize_Chart), 3, Data_Name);
      Bind_Int(Self.Statements(Stmt_Initialize_Chart), 4, Aggregation_Interval);
      Step(Self.Statements(Stmt_Initialize_Chart));
      Reset(Self.Statements(Stmt_Initialize_Chart));
   end Initialize_Chart;
   
   procedure Add_Chart_Setup
     (Self : in out Chart_Database;
      Chart_Name : String;
      Setup_Start_Time : Ada.Calendar.Time;
      Setup_End_Time : Ada.Calendar.Time)
   is
      use Ada_Sqlite3;
      Creation_Time : constant Integer := To_Unix_Time(Ada.Calendar.Clock);
   begin
      Bind_Text(Self.Statements(Stmt_Insert_Setup), 1, Chart_Name);
      Bind_Int(Self.Statements(Stmt_Insert_Setup), 2, To_Unix_Time(Setup_Start_Time));
      Bind_Int(Self.Statements(Stmt_Insert_Setup), 3, To_Unix_Time(Setup_End_Time));
      Bind_Int(Self.Statements(Stmt_Insert_Setup), 4, Creation_Time);
      Step(Self.Statements(Stmt_Insert_Setup));
      Reset(Self.Statements(Stmt_Insert_Setup));
   end Add_Chart_Setup;
   
   procedure Set_Transformation
     (Self : in out Chart_Database;
      Chart_Name : String;
      Transform : Transformation_Type)
   is
      use Ada_Sqlite3;
   begin
      Bind_Text(Self.Statements(Stmt_Set_Transformation), 1, Chart_Name);
      Bind_Text(Self.Statements(Stmt_Set_Transformation), 2, To_String(Transform));
      Step(Self.Statements(Stmt_Set_Transformation));
      Reset(Self.Statements(Stmt_Set_Transformation));
   end Set_Transformation;
   
   function Get_Chart_Data
     (Self : Chart_Database;
      Chart_Name : String;
      Start_Time : Ada.Calendar.Time;
      End_Time : Ada.Calendar.Time) return Chart_Data_Type
   is
      use Ada_Sqlite3;
      use Chart_Types.Observation_Vectors;
      
      Unix_Start : constant Integer := To_Unix_Time(Start_Time);
      Unix_End : constant Integer := To_Unix_Time(End_Time);
      Result : Chart_Data_Type;
      Observations : Vector;
   begin
      -- Get control limits
      Bind_Text(Self.Statements(Stmt_Get_Control_Limits), 1, Chart_Name);
      Step(Self.Statements(Stmt_Get_Control_Limits));
      
      Result.Control_Limits := 
        (Individuals_Mean => Float(Column_Double(Self.Statements(Stmt_Get_Control_Limits), 1)),
         Upper_Individuals_Limit => Float(Column_Double(Self.Statements(Stmt_Get_Control_Limits), 2)),
         Lower_Individuals_Limit => Float(Column_Double(Self.Statements(Stmt_Get_Control_Limits), 3)),
         Cusum_Limit => Float(Column_Double(Self.Statements(Stmt_Get_Control_Limits), 4)));
      Reset(Self.Statements(Stmt_Get_Control_Limits));
      
      -- Get chart points
      Bind_Text(Self.Statements(Stmt_Get_Chart_Points), 1, Chart_Name);
      Bind_Int(Self.Statements(Stmt_Get_Chart_Points), 2, Unix_Start);
      Bind_Int(Self.Statements(Stmt_Get_Chart_Points), 3, Unix_End);
      
      -- Process points
      while Step(Self.Statements(Stmt_Get_Chart_Points)) = ROW loop
         declare
            Point : Observation_Type;
            Annotations_Text : constant String := Column_Text(Self.Statements(Stmt_Get_Chart_Points), 5);
         begin
            Point.ID := Column_Int(Self.Statements(Stmt_Get_Chart_Points), 0);
            Point.Time := From_Unix_Time(Column_Int(Self.Statements(Stmt_Get_Chart_Points), 1));
            Point.Individuals_Value := Float(Column_Double(Self.Statements(Stmt_Get_Chart_Points), 2));
            Point.CUSUM := 
              (Upper_Statistic => Float(Column_Double(Self.Statements(Stmt_Get_Chart_Points), 3)),
               Lower_Statistic => Float(Column_Double(Self.Statements(Stmt_Get_Chart_Points), 4)));
            Point.Is_Setup := False;  -- TODO: Determine from setup times
            Point.Annotations := To_Unbounded_String(Annotations_Text);
            
            Append(Observations, Point);
         end;
      end loop;
      
      Result.Observations := Observations;
      return Result;
   end Get_Chart_Data;

end Chart_DB;
