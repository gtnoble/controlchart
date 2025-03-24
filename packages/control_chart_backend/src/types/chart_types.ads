with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Chart_Types is
   type Chart_Type is (Individuals, Counts);

   type Control_Limits_Type is record
      Individuals_Mean        : Float;
      Upper_Individuals_Limit : Float;
      Lower_Individuals_Limit : Float;
      Cusum_Limit            : Float;
   end record;

   type Statistics_Tests_Type is record
      Runs_Random : Float;
      KS_Normal   : Float;
   end record;

   type CUSUM_Statistics_Type is record
      Upper_Statistic : Float;
      Lower_Statistic : Float;
   end record;

   type Observation_Type is record
      ID               : Integer;
      Individuals_Value : Float;
      CUSUM            : CUSUM_Statistics_Type;
      Time             : Ada.Calendar.Time;
      Is_Setup         : Boolean;
      Annotations      : Unbounded_String;  -- Comma-separated annotations
   end record;

   package Observation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Observation_Type);

   type Chart_Data_Type is record
      Chart_Type      : Chart_Type;
      Tests           : Statistics_Tests_Type;
      Control_Limits  : Control_Limits_Type;
      Observations    : Observation_Vectors.Vector;
   end record;

   type Transformation_Type is (None, Log_Transform, Anscombe_Transform, Freeman_Tukey_Transform);

   -- Type conversions
   function To_Chart_Type (S : String) return Chart_Type;
   function To_String (CT : Chart_Type) return String;

   function To_Transformation_Type (S : String) return Transformation_Type;
   function To_String (T : Transformation_Type) return String;

end Chart_Types;
