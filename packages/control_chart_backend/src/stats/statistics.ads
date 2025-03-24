package Statistics is
   -- Statistical Functions
   type Float_Array is array (Natural range <>) of Float;
   
   function Runs_Randomness_Test (Values : Float_Array) return Float;
   
   -- CUSUM Functions
   function CUSUM_Positive_S
     (Value    : Float;
      Mean     : Float;
      K        : Float;
      Previous : Float := 0.0) return Float;
      
   function CUSUM_Negative_S
     (Value    : Float;
      Mean     : Float;
      K        : Float;
      Previous : Float := 0.0) return Float;
   
   -- Standard Deviation calculation
   function Standard_Deviation (Values : Float_Array) return Float;
   function C4 (N : Positive) return Float;
   
   -- Constants for CUSUM calculation
   Delta_K : constant Float := 1.0;
   Alpha_K : constant Float := 0.0027;
   Beta_K  : constant Float := 0.01;
   
   -- Calculate CUSUM parameters K and H
   function Calculate_CUSUM_K (Std_Dev : Float) return Float;
   function Calculate_CUSUM_H (K : Float) return Float;
   
private
   -- Helper functions
   function Mean (Values : Float_Array) return Float;
   function Sort (Values : Float_Array) return Float_Array;
   function Calculate_Ranks (Values : Float_Array) return Float_Array;
   
   -- Log Gamma function
   function Log_Gamma (X : Float) return Float;
end Statistics;
