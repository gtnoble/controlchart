with Ada.Strings.Unbounded;
with AWS.Status;
with AWS.Response;
with AWS.Parameters;

with Chart_DB;

package Server is
   
   type Server_Type is tagged private;
   
   -- Callback handling
   function Request_Callback 
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
   
   -- Initialize and start server
   function Create
     (Database_Path : String;
      Host         : String := "localhost";
      Port         : Positive := 3000) return Server_Type;
      
   procedure Start (Self : in out Server_Type);
   procedure Stop (Self : in out Server_Type);
   
   -- HTTP handlers
   function Get_Available_Charts 
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Get_Available_Data_Names
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Get_Chart
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Get_Recent_Chart
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Create_Chart
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Add_Observation
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Add_Chart_Setup
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Add_Annotation
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   function Get_Annotation
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data;
      
   -- Error handling
   Server_Error : exception renames Chart_DB.Database_Error;
   
private
   
   type Server_Type is tagged record
      Database : Chart_DB.Chart_Database;
      Host    : Ada.Strings.Unbounded.Unbounded_String;
      Port    : Positive;
   end record;
   
   -- Helper functions for parameter parsing
   function Get_Required_Param
     (Params : AWS.Parameters.List;
      Name   : String) return String;
      
   function Get_Optional_Param
     (Params : AWS.Parameters.List;
      Name   : String;
      Default : String := "") return String;
      
   -- Helper functions for response generation
   function Create_JSON_Response
     (Content : String;
      Status  : Natural := 200) return AWS.Response.Data;
      
   function Create_Error_Response
     (Message : String;
      Status  : Natural := 400) return AWS.Response.Data;
   
end Server;
