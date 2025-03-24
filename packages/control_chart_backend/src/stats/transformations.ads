with Ada.Numerics.Generic_Elementary_Functions;

package Transformations is
   type Transform_Function is access function (X : Float) return Float;
   type Transform_Derivative is access function (X : Float) return Float;

   -- Newton's method parameters
   Absolute_Tolerance : constant Float := 0.0001;
   Relative_Tolerance : constant Float := 0.0001;
   Max_Iterations    : constant Positive := 100;
   
   -- Forward transformations
   function Log_Forward (X : Float) return Float;
   function Anscombe_Forward (X : Float) return Float;
   function Freeman_Tukey_Forward (X : Float) return Float;
   
   -- Inverse transformations
   function Log_Reverse (Y : Float) return Float;
   function Anscombe_Reverse (Y : Float) return Float;
   function Freeman_Tukey_Reverse (Y : Float) return Float;
   
   -- Derivatives for Newton's method
   function Freeman_Tukey_Derivative (X : Float) return Float;
   
private
   -- Generic numerical methods
   function Newton_Method
     (F     : not null Transform_Function;
      DF    : not null Transform_Derivative;
      Guess : Float) return Float;
      
   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Elementary_Functions;
   
end Transformations;
