with Ada.Calendar;
with Ada.Strings.Unbounded;
with Chart_Types; use Chart_Types;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package JSON_Types is
   -- Request types
   type Create_Chart_Request is record
      Chart_Name : Ada.Strings.Unbounded.Unbounded_String;
      Data_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Chart_Type : Chart_Types.Chart_Type;
      Aggregation_Interval : Integer;
      Transformation : Chart_Types.Transformation_Type;
   end record;
   
   type Observation_Request is record
      Value       : Float;
      Data_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Time        : Ada.Calendar.Time;
      Annotations : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   
   type Annotation_Request is record
      Data_Point_ID : Integer;
      Annotation    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   
   type Chart_Setup_Request is record
      Chart_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Start_Time  : Ada.Calendar.Time;
      End_Time    : Ada.Calendar.Time;
   end record;
   
   -- JSON conversion functions
   function To_JSON (Chart_Data : Chart_Data_Type) return JSON_Value;
   function To_JSON (Limits : Control_Limits_Type) return JSON_Value;
   function To_JSON (Stats : Statistics_Tests_Type) return JSON_Value;
   function To_JSON (Observation : Observation_Type) return JSON_Value;
   
   function From_JSON (Value : JSON_Value) return Create_Chart_Request;
   function From_JSON (Value : JSON_Value) return Observation_Request;
   function From_JSON (Value : JSON_Value) return Annotation_Request;
   function From_JSON (Value : JSON_Value) return Chart_Setup_Request;
   
   -- Convenience functions for string conversion
   function To_JSON_String (Value : JSON_Value) return String;
   function Parse_JSON_String (JSON_String : String) return JSON_Value;

private
   -- Helper functions for time handling
   function Format_Time (Time : Ada.Calendar.Time) return String;
   function Parse_Time (Time_String : String) return Ada.Calendar.Time;
   
   -- Helper functions for JSON array handling
   function Create_JSON_Array (Values : Observation_Vectors.Vector) return JSON_Array;
   
end JSON_Types;
