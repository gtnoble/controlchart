with Statistics;

package body Chart_DB.Functions is
   
   function StdDev_Step
     (Args : StdDev_Functions.Function_Args;
      Context : StdDev_Context) return StdDev_Functions.Result_Type 
   is
      use StdDev_Functions;
      
      X : constant Long_Float := Get_Double(Args, 0);
      N1 : constant Natural := Context.N + 1;
      Value_Delta : constant Long_Float := X - Context.M1;
      Value_Delta_N : constant Long_Float := Value_Delta / Long_Float(N1);
      Next_M1 : constant Long_Float := Context.M1 + Value_Delta_N;
      Next_M2 : constant Long_Float := Context.M2 + Value_Delta * Value_Delta_N * Long_Float(N1 - 1);
   begin
      return Result_Type'(
        Kind => Double_Result,
        Double_Value => Next_M1);
   end StdDev_Step;
   
   procedure StdDev_Inverse
     (Args : StdDev_Functions.Function_Args;
      Context : in out StdDev_Context) 
   is
      use StdDev_Functions;
      
      X : constant Long_Float := Get_Double(Args, 0);
      N1 : constant Natural := Context.N - 1;
      Value_Delta : constant Long_Float := X - Context.M1;
      Value_Delta_N : constant Long_Float := Value_Delta / Long_Float(Context.N);
   begin
      if N1 > 0 then
         Context.M1 := Context.M1 - Value_Delta_N;
         Context.M2 := Context.M2 - Value_Delta * Value_Delta_N * Long_Float(Context.N - 1);
      else
         Context.M1 := 0.0;
         Context.M2 := 0.0;
      end if;
      Context.N := N1;
   end StdDev_Inverse;
   
   function StdDev_Value
     (Context : StdDev_Context) return StdDev_Functions.Result_Type 
   is
      use StdDev_Functions;
   begin
      if Context.N < 2 then
         return Result_Type'(Kind => Null_Result);
      end if;
      
      return Result_Type'(
        Kind => Double_Result,
        Double_Value => Sqrt(Context.M2 / Long_Float(Context.N - 1)) / Long_Float(Statistics.C4(Context.N)));
   end StdDev_Value;
   
   function StdDev_Final
     (Context : StdDev_Context) return StdDev_Functions.Result_Type 
   is
   begin
      return StdDev_Value(Context);
   end StdDev_Final;
   
   function CUSUM_Upper_Step
     (Args : CUSUM_Functions.Function_Args;
      Context : CUSUM_Context) return CUSUM_Functions.Result_Type 
   is
      use CUSUM_Functions;
      
      X : constant Long_Float := Get_Double(Args, 0);
      Mean : constant Long_Float := Get_Double(Args, 1);
      K : constant Long_Float := Get_Double(Args, 2);
      Next_S : constant Long_Float := Long_Float'Max(0.0, Context.S + (X - Mean) - K);
   begin
      return Result_Type'(
        Kind => Double_Result,
        Double_Value => Next_S);
   end CUSUM_Upper_Step;
   
   function CUSUM_Lower_Step
     (Args : CUSUM_Functions.Function_Args;
      Context : CUSUM_Context) return CUSUM_Functions.Result_Type 
   is
      use CUSUM_Functions;
      
      X : constant Long_Float := Get_Double(Args, 0);
      Mean : constant Long_Float := Get_Double(Args, 1);
      K : constant Long_Float := Get_Double(Args, 2);
      Next_S : constant Long_Float := Long_Float'Max(0.0, Context.S - (X - Mean) - K);
   begin
      return Result_Type'(
        Kind => Double_Result,
        Double_Value => Next_S);
   end CUSUM_Lower_Step;
   
   function CUSUM_Value
     (Context : CUSUM_Context) return CUSUM_Functions.Result_Type 
   is
      use CUSUM_Functions;
   begin
      return Result_Type'(
        Kind => Double_Result,
        Double_Value => Context.S);
   end CUSUM_Value;
   
   function CUSUM_Final
     (Context : CUSUM_Context) return CUSUM_Functions.Result_Type 
   is
   begin
      return CUSUM_Value(Context);
   end CUSUM_Final;
   
   procedure CUSUM_Inverse
     (Args : CUSUM_Functions.Function_Args;
      Context : in out CUSUM_Context) 
   is
      use CUSUM_Functions;
      
      X : constant Long_Float := Get_Double(Args, 0);
      Mean : constant Long_Float := Get_Double(Args, 1);
      K : constant Long_Float := Get_Double(Args, 2);
   begin
      Context.S := Long_Float'Max(0.0, Context.S - ((X - Mean) - K));
   end CUSUM_Inverse;
   
   procedure Register_Functions
     (DB : in out Ada_Sqlite3.Database) 
   is
      use StdDev_Functions;
      use CUSUM_Functions;
      
      Initial_StdDev_Context : constant StdDev_Context := 
        (N => 0, M1 => 0.0, M2 => 0.0);
      Initial_CUSUM_Context : constant CUSUM_Context :=
        (S => 0.0);
   begin
      -- Register standard deviation window function
      Create_Window(
        DB => DB,
        Name => "standardDeviation",
        N_Args => 1,
        Step_Func => StdDev_Step'Access,
        Final_Func => StdDev_Final'Access,
        Value_Func => StdDev_Value'Access,
        Inverse_Func => StdDev_Inverse'Access,
        Context => Initial_StdDev_Context,
        Flags => Deterministic);
        
      -- Register CUSUM upper window function
      Create_Window(
        DB => DB,
        Name => "cusumPositiveS",
        N_Args => 3,
        Step_Func => CUSUM_Upper_Step'Access,
        Final_Func => CUSUM_Final'Access,
        Value_Func => CUSUM_Value'Access,
        Inverse_Func => CUSUM_Inverse'Access,
        Context => Initial_CUSUM_Context,
        Flags => Deterministic);
        
      -- Register CUSUM lower window function
      Create_Window(
        DB => DB,
        Name => "cusumNegativeS",
        N_Args => 3,
        Step_Func => CUSUM_Lower_Step'Access,
        Final_Func => CUSUM_Final'Access,
        Value_Func => CUSUM_Value'Access,
        Inverse_Func => CUSUM_Inverse'Access,
        Context => Initial_CUSUM_Context,
        Flags => Deterministic);
   end Register_Functions;

end Chart_DB.Functions;
