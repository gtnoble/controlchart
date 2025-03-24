with Ada.Strings.Fixed;
with Ada.Characters.Handling;

package body Chart_Types is
   
   function To_Chart_Type (S : String) return Chart_Type is
      use Ada.Characters.Handling;
      Upper_S : constant String := To_Upper (S);
   begin
      if Upper_S = "INDIVIDUALS" then
         return Individuals;
      elsif Upper_S = "COUNTS" then
         return Counts;
      else
         raise Constraint_Error with "Invalid chart type: " & S;
      end if;
   end To_Chart_Type;
   
   function To_String (CT : Chart_Type) return String is
   begin
      case CT is
         when Individuals => return "individuals";
         when Counts     => return "counts";
      end case;
   end To_String;
   
   function To_Transformation_Type (S : String) return Transformation_Type is
      use Ada.Characters.Handling;
      Upper_S : constant String := To_Upper (S);
   begin
      if Upper_S = "LOG" then
         return Log_Transform;
      elsif Upper_S = "ANSCOMBE" then
         return Anscombe_Transform;
      elsif Upper_S = "FREEMAN-TUKEY" or else Upper_S = "FREEMAN_TUKEY" then
         return Freeman_Tukey_Transform;
      elsif Upper_S = "NONE" or else S = "" then
         return None;
      else
         raise Constraint_Error with "Invalid transformation type: " & S;
      end if;
   end To_Transformation_Type;
   
   function To_String (T : Transformation_Type) return String is
   begin
      case T is
         when None                => return "";
         when Log_Transform       => return "log";
         when Anscombe_Transform  => return "anscombe";
         when Freeman_Tukey_Transform => return "freeman-tukey";
      end case;
   end To_String;
   
end Chart_Types;
