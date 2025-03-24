with Chart_DB.SQL_Statements;

package body Chart_DB.Database_Types is
   
   procedure Prepare_Statements
     (DB : in out Ada_Sqlite3.Database;
      Stmts : in out Prepared_Statements)
   is
      use Ada_Sqlite3;
      use SQL_Statements;
   begin
      -- Prepare each statement
      Stmts(Stmt_Insert_Observation) := Prepare(DB, Insert_Observation);
      Stmts(Stmt_Insert_Annotation) := Prepare(DB, Insert_Annotation);
      Stmts(Stmt_Initialize_Chart) := Prepare(DB, Initialize_Chart);
      Stmts(Stmt_Insert_Setup) := Prepare(DB, Insert_Setup);
      Stmts(Stmt_Set_Transformation) := Prepare(DB, Set_Transformation);
      Stmts(Stmt_Get_Chart_Points) := Prepare(DB, Get_Chart_Points);
      Stmts(Stmt_Get_Control_Limits) := Prepare(DB, Get_Control_Limits);
   end Prepare_Statements;
   
   procedure Finalize_Statements
     (Stmts : in out Prepared_Statements)
   is
   begin
      null;  -- SQLite handles statement cleanup automatically
   end Finalize_Statements;

end Chart_DB.Database_Types;
