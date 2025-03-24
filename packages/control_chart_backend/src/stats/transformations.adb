with Ada.Numerics;

package body Transformations is
   
   function Log_Forward (X : Float) return Float is
   begin
      return Log (X, 10.0);
   end Log_Forward;
   
   function Log_Reverse (Y : Float) return Float is
   begin
      return 10.0 ** Y;
   end Log_Reverse;
   
   function Anscombe_Forward (X : Float) return Float is
   begin
      return 2.0 * Sqrt (X);
   end Anscombe_Forward;
   
   function Anscombe_Reverse (Y : Float) return Float is
      Half_Y : constant Float := Y / 2.0;
   begin
      return Half_Y * Half_Y;
   end Anscombe_Reverse;
   
   function Freeman_Tukey_Forward (X : Float) return Float is
   begin
      return Sqrt (X + 1.0) + Sqrt (X);
   end Freeman_Tukey_Forward;
   
   function Freeman_Tukey_Derivative (X : Float) return Float is
   begin
      return 1.0 / (2.0 * Sqrt (X + 1.0)) + 1.0 / (2.0 * Sqrt (X));
   end Freeman_Tukey_Derivative;
   
   function Freeman_Tukey_Reverse (Y : Float) return Float is
      -- Create local functions for Newton's method
      function F (X : Float) return Float is
      begin
         return Freeman_Tukey_Forward (X) - Y;
      end F;
      
      -- Use Anscombe transform as initial guess
      Initial_Guess : constant Float := Anscombe_Reverse (Y);
   begin
      return Newton_Method (F'Access, Freeman_Tukey_Derivative'Access, Initial_Guess);
   end Freeman_Tukey_Reverse;
   
   function Newton_Method
     (F     : not null Transform_Function;
      DF    : not null Transform_Derivative;
      Guess : Float) return Float 
   is
      Xn    : Float := Guess;
      Step_Size : Float := Float'Last;
      
      function Is_Converged return Boolean is
         Abs_Step : constant Float := abs Step_Size;
         Rel_Step : constant Float := (if Xn /= 0.0 then Abs_Step / abs Xn else Float'Last);
      begin
         return Abs_Step < Absolute_Tolerance or else
                Rel_Step < Relative_Tolerance;
      end Is_Converged;
   begin
      for Iter in 1 .. Max_Iterations loop
         declare
            F_Val  : constant Float := F (Xn);
            DF_Val : constant Float := DF (Xn);
            Next_Xn : Float;
         begin
            -- Check for division by zero
            if DF_Val = 0.0 then
               return Xn;  -- Return current value if derivative is zero
            end if;
            
            Next_Xn := Xn - F_Val / DF_Val;
            
            -- Check for NaN
            if Next_Xn /= Next_Xn then  -- NaN check
               return Xn;
            end if;
            
            Step_Size := Next_Xn - Xn;
            Xn := Next_Xn;
            
            exit when Is_Converged;
         end;
      end loop;
      
      return Xn;
   end Newton_Method;
   
end Transformations;
