with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

package body JSON_Types is
   use Ada.Strings.Unbounded;
   
   function Format_Time (Time : Ada.Calendar.Time) return String is
      use Ada.Calendar.Formatting;
      Milliseconds : constant Integer := Integer (Sub_Second (Time) * 1000.0);
   begin
      return Image (Time, Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset) &
             '.' & Trim (Milliseconds'Image, Ada.Strings.Left);
   end Format_Time;
   
   function Parse_Time (Time_String : String) return Ada.Calendar.Time is
      use Ada.Calendar.Formatting;
   begin
      return Value (Time_String);
   end Parse_Time;
   
   function To_JSON_String (Value : JSON_Value) return String is
   begin
      return Write (Value);
   end To_JSON_String;
   
   function Parse_JSON_String (JSON_String : String) return JSON_Value is
   begin
      return Read (JSON_String);
   end Parse_JSON_String;
   
   function Create_JSON_Array (Values : Observation_Vectors.Vector) return JSON_Array is
      Result : JSON_Array;
   begin
      for Value of Values loop
         Append (Result, To_JSON (Value));
      end loop;
      return Result;
   end Create_JSON_Array;
   
   -- JSON conversion functions
   function To_JSON (Chart_Data : Chart_Data_Type) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("type", To_String (Chart_Data.Chart_Type));
      Result.Set_Field ("tests", To_JSON (Chart_Data.Tests));
      Result.Set_Field ("controlLimits", To_JSON (Chart_Data.Control_Limits));
      Result.Set_Field ("observations", Create_JSON_Array (Chart_Data.Observations));
      return Result;
   end To_JSON;
   
   function To_JSON (Limits : Control_Limits_Type) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("individualsMean", Limits.Individuals_Mean);
      Result.Set_Field ("upperIndividualsLimit", Limits.Upper_Individuals_Limit);
      Result.Set_Field ("lowerIndividualsLimit", Limits.Lower_Individuals_Limit);
      Result.Set_Field ("cusumLimit", Limits.Cusum_Limit);
      return Result;
   end To_JSON;
   
   function To_JSON (Stats : Statistics_Tests_Type) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("runsRandom", Stats.Runs_Random);
      Result.Set_Field ("ksNormal", Stats.KS_Normal);
      return Result;
   end To_JSON;
   
   function To_JSON (Observation : Observation_Type) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
      CUSUM_Object : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("id", Observation.ID);
      Result.Set_Field ("individualsValue", Observation.Individuals_Value);
      
      CUSUM_Object.Set_Field ("upperStatistic", Observation.CUSUM.Upper_Statistic);
      CUSUM_Object.Set_Field ("lowerStatistic", Observation.CUSUM.Lower_Statistic);
      Result.Set_Field ("cusum", CUSUM_Object);
      
      Result.Set_Field ("time", Format_Time (Observation.Time));
      Result.Set_Field ("isSetup", Observation.Is_Setup);
      Result.Set_Field ("annotations", To_String (Observation.Annotations));
      return Result;
   end To_JSON;
   
   -- JSON parsing functions
   function From_JSON (Value : JSON_Value) return Create_Chart_Request is
   begin
      return (
         Chart_Name => To_Unbounded_String (Value.Get ("chartName")),
         Data_Name => To_Unbounded_String (Value.Get ("dataName")),
         Chart_Type => To_Chart_Type (Value.Get ("chartType")),
         Aggregation_Interval => Value.Get ("aggregationInterval"),
         Transformation => To_Transformation_Type (Value.Get ("transformation"))
      );
   end From_JSON;
   
   function From_JSON (Value : JSON_Value) return Observation_Request is
   begin
      return (
         Value => Value.Get ("value"),
         Data_Name => To_Unbounded_String (Value.Get ("dataName")),
         Time => Parse_Time (Value.Get ("time")),
         Annotations => To_Unbounded_String (Value.Get ("annotations"))
      );
   end From_JSON;
   
   function From_JSON (Value : JSON_Value) return Annotation_Request is
   begin
      return (
         Data_Point_ID => Value.Get ("dataPointId"),
         Annotation => To_Unbounded_String (Value.Get ("annotation"))
      );
   end From_JSON;
   
   function From_JSON (Value : JSON_Value) return Chart_Setup_Request is
   begin
      return (
         Chart_Name => To_Unbounded_String (Value.Get ("chartName")),
         Start_Time => Parse_Time (Value.Get ("startTime")),
         End_Time => Parse_Time (Value.Get ("endTime"))
      );
   end From_JSON;
   
end JSON_Types;
