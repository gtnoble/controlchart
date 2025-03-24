with Ada_Sqlite3;

package Chart_DB.Database_Types is

   type Prepared_Statement_Type is 
     (Stmt_Insert_Observation,
      Stmt_Insert_Annotation,
      Stmt_Initialize_Chart,
      Stmt_Insert_Setup,
      Stmt_Set_Transformation,
      Stmt_Get_Chart_Points,
      Stmt_Get_Control_Limits);
   
   type Prepared_Statements is array (Prepared_Statement_Type) of Ada_Sqlite3.Statement;
   
   -- Exception for database errors
   Database_Error : exception;
   
   -- Represents a point in the database
   type Chart_Point_Data is record
      ID : Integer;
      Time : Integer;
      Value : Long_Float;
      CUSUM_Upper : Long_Float;
      CUSUM_Lower : Long_Float;
      Annotations : String;
   end record;
   
   -- Control limits from database
   type Control_Limits_Data is record
      Mean : Long_Float;
      Upper_Limit : Long_Float;
      Lower_Limit : Long_Float;
      CUSUM_Limit : Long_Float;
   end record;
   
   -- Prepare all statements
   procedure Prepare_Statements
     (DB : in out Ada_Sqlite3.Database;
      Stmts : in out Prepared_Statements);
   
   -- Helper for statement cleanup
   procedure Finalize_Statements
     (Stmts : in out Prepared_Statements);

end Chart_DB.Database_Types;
