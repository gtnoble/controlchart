with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Long_Long_Elementary_Functions;


package body Statistics is
   package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Float_Functions;
   
   -- Constants for Lanczos approximation
   G : constant := 7.0;  -- Number of coefficients
   P : constant array (0 .. 6) of Float := (
      0.99999999999980993,
      676.5203681218851,
      -1259.1392167224028,
      771.32342877765313,
      -176.61502916214059,
      12.507343278686905,
      -0.13857109526572012
   );
   
   function Log_Gamma (X : Float) return Float is
      Z : Float;
      Sum : Float;
   begin
      if X <= 0.0 then
         return 0.0;  -- Return 0 for non-positive inputs
      end if;
      
      Z := X - 1.0;
      Sum := P(0);
      
      for I in 1 .. 6 loop
         Sum := Sum + P(I) / (Z + Float(I));
      end loop;
      
      -- Calculate main terms of Lanczos approximation
      return (X + G - 0.5) * Log(X + G - 0.5) - (X + G - 0.5) +
             Log(2.0 * Ada.Numerics.Pi) / 2.0 +
             Log(Sum) - Log(X);
   end Log_Gamma;
   
   
   function Mean (Values : Float_Array) return Float is
      Sum : Float := 0.0;
   begin
      if Values'Length = 0 then
         return 0.0;
      end if;
      
      for V of Values loop
         Sum := Sum + V;
      end loop;
      
      return Sum / Float (Values'Length);
   end Mean;
   
   function Standard_Deviation (Values : Float_Array) return Float is
      N    : constant Float := Float (Values'Length);
      Mean_Val : constant Float := Mean (Values);
      Sum : Float := 0.0;
   begin
      if Values'Length < 2 then
         return 0.0;
      end if;
      
      for V of Values loop
         Sum := Sum + (V - Mean_Val) ** 2;
      end loop;
      
      return Sqrt (Sum / (N - 1.0)) / C4 (Values'Length);
   end Standard_Deviation;
   
   function C4 (N : Positive) return Float is
      N_Float : constant Float := Float (N);
   begin
      if N <= 1 then
         return 1.0;
      end if;
      
      return Sqrt (2.0 / (N_Float - 1.0)) *
             Exp(Log_Gamma(N_Float / 2.0) - Log_Gamma((N_Float - 1.0) / 2.0));
   end C4;
   
   function CUSUM_Positive_S
     (Value    : Float;
      Mean     : Float;
      K        : Float;
      Previous : Float := 0.0) return Float 
   is
      Candidate : constant Float := Previous + Value - Mean - K;
   begin
      if Candidate > 0.0 then
         return Candidate;
      else
         return 0.0;
      end if;
   end CUSUM_Positive_S;
   
   function CUSUM_Negative_S
     (Value    : Float;
      Mean     : Float;
      K        : Float;
      Previous : Float := 0.0) return Float 
   is
      Candidate : constant Float := Previous - Value + Mean - K;
   begin
      if Candidate > 0.0 then
         return Candidate;
      else
         return 0.0;
      end if;
   end CUSUM_Negative_S;
   
   function Calculate_CUSUM_K (Std_Dev : Float) return Float is
   begin
      return Std_Dev * Delta_K / 2.0;
   end Calculate_CUSUM_K;
   
   function Calculate_CUSUM_H (K : Float) return Float is
   begin
      return K * 2.0 / (Delta_K * Delta_K) * Log ((1.0 - Beta_K) / Alpha_K);
   end Calculate_CUSUM_H;
   
   function Sort (Values : Float_Array) return Float_Array is
      Result : Float_Array := Values;
      
      procedure Swap (Left, Right : in out Float) is
         Temp : constant Float := Left;
      begin
         Left := Right;
         Right := Temp;
      end Swap;
      
      -- Simple bubble sort for now
      -- TODO: Replace with more efficient algorithm if needed
   begin
      for I in Result'First .. Result'Last - 1 loop
         for J in Result'First .. Result'Last - 1 loop
            if Result (J) > Result (J + 1) then
               Swap (Result (J), Result (J + 1));
            end if;
         end loop;
      end loop;
      return Result;
   end Sort;
   
   function Calculate_Ranks (Values : Float_Array) return Float_Array is
      Sorted : constant Float_Array := Sort (Values);
      Result : Float_Array (Values'Range);
      Current_Rank : Float := 1.0;
      I : Natural := Values'First;
   begin
      while I <= Values'Last loop
         -- Find all equal values and assign average rank
         declare
            Equal_Count : Natural := 1;
            Value : constant Float := Values (I);
         begin
            for J in I + 1 .. Values'Last loop
               exit when Values (J) /= Value;
               Equal_Count := Equal_Count + 1;
            end loop;
            
            -- Assign average rank to all equal values
            declare
               Average_Rank : constant Float := 
                 (Current_Rank * 2.0 + Float (Equal_Count - 1)) / 2.0;
            begin
               for J in 0 .. Equal_Count - 1 loop
                  Result (I + J) := Average_Rank;
               end loop;
            end;
            
            I := I + Equal_Count;
            Current_Rank := Current_Rank + Float (Equal_Count);
         end;
      end loop;
      
      return Result;
   end Calculate_Ranks;
   
   function Runs_Randomness_Test (Values : Float_Array) return Float is
      Mean_Val : constant Float := Mean (Values);
      Runs : Natural := 1;
      Above : Boolean := Values (Values'First) > Mean_Val;
   begin
      if Values'Length < 2 then
         return 1.0;
      end if;
      
      for I in Values'First + 1 .. Values'Last loop
         declare
            Current_Above : constant Boolean := Values (I) > Mean_Val;
         begin
            if Current_Above /= Above then
               Runs := Runs + 1;
               Above := Current_Above;
            end if;
         end;
      end loop;
      
      -- TODO: Implement proper p-value calculation
      -- For now, return a simplified metric
      return Float (Runs) / Float (Values'Length);
   end Runs_Randomness_Test;
   
end Statistics;
