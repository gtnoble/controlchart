with Ada.Numerics.Generic_Elementary_Functions;
with Ada_Sqlite3.Generic_Functions;

package Chart_DB.Functions is
   
   -- Context types for aggregate functions
   type StdDev_Context is record
      N : Natural := 0;
      M1 : Long_Float := 0.0;  -- First moment (mean)
      M2 : Long_Float := 0.0;  -- Second moment
   end record;
   
   type CUSUM_Context is record
      S : Long_Float := 0.0;  -- Current CUSUM statistic
   end record;
   
   -- Function packages instantiations
   package StdDev_Functions is new Ada_Sqlite3.Generic_Functions
     (Context_Type => StdDev_Context);
     
   package CUSUM_Functions is new Ada_Sqlite3.Generic_Functions
     (Context_Type => CUSUM_Context);
   
   -- Function registration procedures
   procedure Register_Functions
     (DB : in out Ada_Sqlite3.Database);
   
private
   -- Standard Deviation window functions
   function StdDev_Step
     (Args : StdDev_Functions.Function_Args;
      Context : StdDev_Context) return StdDev_Functions.Result_Type;
      
   function StdDev_Final
     (Context : StdDev_Context) return StdDev_Functions.Result_Type;
     
   function StdDev_Value
     (Context : StdDev_Context) return StdDev_Functions.Result_Type;
     
   procedure StdDev_Inverse
     (Args : StdDev_Functions.Function_Args;
      Context : StdDev_Context);
   
   -- CUSUM window functions
   function CUSUM_Upper_Step
     (Args : CUSUM_Functions.Function_Args;
      Context : CUSUM_Context) return CUSUM_Functions.Result_Type;
      
   function CUSUM_Lower_Step
     (Args : CUSUM_Functions.Function_Args;
      Context : CUSUM_Context) return CUSUM_Functions.Result_Type;
      
   function CUSUM_Final
     (Context : CUSUM_Context) return CUSUM_Functions.Result_Type;
     
   function CUSUM_Value
     (Context : CUSUM_Context) return CUSUM_Functions.Result_Type;
     
   procedure CUSUM_Inverse
     (Args : CUSUM_Functions.Function_Args;
      Context : CUSUM_Context);
   
   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Float_Type => Long_Float);
   use Elementary_Functions;

end Chart_DB.Functions;
