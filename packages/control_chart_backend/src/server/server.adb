with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response.Set;
with JSON_Types; use JSON_Types;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.URL;
with Alog.Active_Logger;

package body Server is
   Web_Server : AWS.Server.HTTP;
   
   -- CORS handling
   function Add_CORS_Headers (Response : AWS.Response.Data) return AWS.Response.Data is
      Result : AWS.Response.Data := Response;
   begin
      AWS.Response.Set.Add_Header
        (Result,
         "Access-Control-Allow-Origin",
         "*");
      AWS.Response.Set.Add_Header
        (Result,
         "Access-Control-Allow-Methods",
         "GET, POST, OPTIONS");
      AWS.Response.Set.Add_Header
        (Result,
         "Access-Control-Allow-Headers",
         "Content-Type");
      return Result;
   end Add_CORS_Headers;
   
   function Handle_Options return AWS.Response.Data is
      Response : AWS.Response.Data;
   begin
      Response := AWS.Response.Build
        (Content_Type => "text/plain",
         Message_Body => "");
      return Add_CORS_Headers (Response);
   end Handle_Options;
   
   -- Request callback
   function Request_Callback
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      URI : constant String := AWS.Status.URI (Request);
      Method : constant String := AWS.Status.Method (Request);
      Response : AWS.Response.Data;
   begin
      -- Log request details
      Alog.Active_Logger.Log
        (Level => Alog.Debug,
         Msg   => "Request: " & Method & " " & URI & 
                  (if Method = "POST" then 
                     " Body: " & AWS.Status.Payload (Request)
                   else
                     ""));
         
      -- Log query parameters for GET requests
      if Method = "GET" then
         declare
            Params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
            Param_Count : constant Natural := AWS.Parameters.Count (Params);
         begin
            if Param_Count > 0 then
               Alog.Active_Logger.Log
                 (Level => Alog.Debug,
                  Msg   => "Parameters: " & AWS.Parameters.URI_Format (Params));
            end if;
         end;
      end if;
      
      -- Handle OPTIONS request for CORS
      if Method = "OPTIONS" then
         return Handle_Options;
      end if;
      
      -- Handle routes based on URI and method
      if Method = "GET" then
         if URI = "/availableCharts" then
            Response := Self.Get_Available_Charts (Request);
         elsif URI = "/availableDataNames" then
            Response := Self.Get_Available_Data_Names (Request);
         elsif URI'Length > 6 and then URI (1..6) = "/chart" then
            if URI'Length > 12 and then URI (URI'Length-11..URI'Length) = "/annotations" then
               Response := Self.Get_Annotation (Request);
            elsif URI'Length > 7 and then URI (URI'Length-6..URI'Length) = "/recent" then
               Response := Self.Get_Recent_Chart (Request);
            else
               Response := Self.Get_Chart (Request);
            end if;
         end if;
      elsif Method = "POST" then
         if URI = "/createChart" then
            Response := Self.Create_Chart (Request);
         elsif URI = "/addObservation" then
            Response := Self.Add_Observation (Request);
         elsif URI'Length > 12 and then URI (URI'Length-11..URI'Length) = "/annotations" then
            Response := Self.Add_Annotation (Request);
         elsif URI'Length > 9 and then URI (URI'Length-8..URI'Length) = "/setSetup" then
            Response := Self.Add_Chart_Setup (Request);
         end if;
      end if;
      
      if Response = AWS.Response.No_Data then
         Alog.Active_Logger.Log
           (Level => Alog.Warning,
            Msg   => "Not Found: " & Method & " " & URI);
         Response := Create_Error_Response
           (Message => "Not Found",
            Status  => AWS.Messages.S404);
      end if;
      
      -- Log response
      declare
         Status_Code : constant Natural := AWS.Response.Status_Code (Response);
         Is_Error : constant Boolean := Status_Code >= 400;
         Log_Level : constant Alog.Level_Type := 
           (if Is_Error then Alog.Warning else Alog.Debug);
      begin
         Alog.Active_Logger.Log
           (Level => Log_Level,
            Msg   => "Response: " & Status_Code'Image & " - " &
                    Method & " " & URI);
         return Add_CORS_Headers (Response);
      end;
   end Request_Callback;
   
   function Create
     (Database_Path : String;
      Host         : String := "localhost";
      Port         : Positive := 3000) return Server_Type 
   is
      DB : Chart_DB.Chart_Database;
   begin
      DB.Open(Database_Path);
      return Server_Type'(
         Database => DB,
         Host     => To_Unbounded_String (Host),
         Port     => Port
      );
   end Create;
   
   procedure Start (Self : in out Server_Type) is
   begin
      Alog.Active_Logger.Log
        (Level => Alog.Info,
         Msg   => "Starting server on port" & Self.Port'Image);
         
      AWS.Server.Start (
         Web_Server => Web_Server,
         Name       => "Control Chart Server",
         Callback   => Self.Request_Callback'Access,
         Port      => Self.Port,
         Max_Connection => 15);
   end Start;
   
   procedure Stop (Self : in out Server_Type) is
      pragma Unreferenced (Self);
   begin
      Alog.Active_Logger.Log
        (Level => Alog.Info,
         Msg   => "Stopping server");
      AWS.Server.Shutdown (Web_Server);
   end Stop;
   
   -- Helper functions
   function Get_Required_Param
     (Params : AWS.Parameters.List;
      Name   : String) return String 
   is
      Value : constant String := AWS.Parameters.Get (Params, Name);
   begin
      if Value = "" then
         raise Server_Error with "Missing required parameter: " & Name;
      end if;
      return Value;
   end Get_Required_Param;
   
   function Get_Optional_Param
     (Params  : AWS.Parameters.List;
      Name    : String;
      Default : String := "") return String 
   is
   begin
      return AWS.Parameters.Get (Params, Name, Default);
   end Get_Optional_Param;
   
   function Create_JSON_Response
     (Content : String;
      Status  : Natural := 200) return AWS.Response.Data 
   is
   begin
      return AWS.Response.Build
        (Content_Type  => "application/json",
         Message_Body  => Content,
         Status_Code   => Status);
   end Create_JSON_Response;
   
   function Create_Error_Response
     (Message : String;
      Status  : Natural := 400) return AWS.Response.Data 
   is
   begin
      return Create_JSON_Response
        (Content => "{""error"": """ & Message & """}",
         Status  => Status);
   end Create_Error_Response;
   
   -- HTTP handlers
   function Get_Available_Charts
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Charts_JSON : String;
   begin
      Charts_JSON := Self.Database.Get_Available_Charts;
      return Create_JSON_Response (Charts_JSON);
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to get available charts: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to get available charts",
            Status  => 500);
   end Get_Available_Charts;
   
   function Get_Available_Data_Names
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Names_JSON : String;
   begin
      Names_JSON := Self.Database.Get_Available_Data_Names;
      return Create_JSON_Response (Names_JSON);
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to get available data names: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to get available data names",
            Status  => 500);
   end Get_Available_Data_Names;
   
   function Get_Chart
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Chart_Name : constant String := Get_Required_Param (Params, "chartName");
      Start_Time : constant String := Get_Required_Param (Params, "startTime");
      End_Time   : constant String := Get_Required_Param (Params, "endTime");
      Transform  : constant Boolean := Boolean'Value (Get_Optional_Param (Params, "transform", "false"));
      
      Chart_Data : Chart_Types.Chart_Data_Type;
      Response_JSON : JSON_Value;
   begin
      Chart_Data := Self.Database.Get_Chart_Data
        (Chart_Name,
         Ada.Calendar.Formatting.Value (Start_Time),
         Ada.Calendar.Formatting.Value (End_Time));
      
      Response_JSON := To_JSON (Chart_Data);
      return Create_JSON_Response (To_JSON_String (Response_JSON));
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to get chart data: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to get chart",
            Status  => 500);
   end Get_Chart;
   
   function Get_Recent_Chart
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Chart_Name : constant String := Get_Required_Param (Params, "chartName");
      Count     : constant Positive := Positive'Value (Get_Required_Param (Params, "count"));
      Transform : constant Boolean := Boolean'Value (Get_Optional_Param (Params, "transform", "false"));
      
      Chart_Data : Chart_Types.Chart_Data_Type;
      Response_JSON : JSON_Value;
   begin
      Chart_Data := Self.Database.Get_Chart_Data
        (Chart_Name,
         Ada.Calendar.Clock - Duration(Count * 60),  -- Last N minutes
         Ada.Calendar.Clock);
      
      Response_JSON := To_JSON (Chart_Data);
      
      return Create_JSON_Response (To_JSON_String (Response_JSON));
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to get recent chart data: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to get recent chart data",
            Status  => 500);
   end Get_Recent_Chart;
   
   function Create_Chart
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Request_JSON : JSON_Value;
      Chart_Request : Create_Chart_Request;
   begin
      Request_JSON := Parse_JSON_String (AWS.Status.Payload (Request));
      Chart_Request := From_JSON (Request_JSON);
      
      Self.Database.Initialize_Chart
        (To_String (Chart_Request.Chart_Name),
         To_String (Chart_Request.Data_Name),
         Chart_Request.Chart_Type,
         Chart_Request.Aggregation_Interval);
         
      if Chart_Request.Transformation /= Chart_Types.None then
         Self.Database.Set_Transformation
           (To_String (Chart_Request.Chart_Name),
            Chart_Request.Transformation);
      end if;
      
      Alog.Active_Logger.Log
        (Level => Alog.Info,
         Msg   => "Created chart: " & To_String (Chart_Request.Chart_Name));
         
      return Create_JSON_Response
        (Content => "{""message"": ""Chart created successfully""}",
         Status  => 201);
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to create chart: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to create chart",
            Status  => 500);
   end Create_Chart;
   
   function Add_Observation
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Request_JSON : JSON_Value;
      Obs_Request : Observation_Request;
   begin
      Request_JSON := Parse_JSON_String (AWS.Status.Payload (Request));
      Obs_Request := From_JSON (Request_JSON);
      
      Self.Database.Save_Observation
        (Value => Obs_Request.Value,
         Data_Name => To_String (Obs_Request.Data_Name),
         Time => Obs_Request.Time,
         Annotations => To_String (Obs_Request.Annotations));
      
      Alog.Active_Logger.Log
        (Level => Alog.Debug,
         Msg   => "Added observation for: " & To_String (Obs_Request.Data_Name));
         
      return Create_JSON_Response
        (Content => "{""message"": ""Observation saved successfully""}",
         Status  => 201);
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to save observation: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to save observation",
            Status  => 500);
   end Add_Observation;
   
   function Add_Chart_Setup
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Request_JSON : JSON_Value;
      Setup_Request : Chart_Setup_Request;
   begin
      Request_JSON := Parse_JSON_String (AWS.Status.Payload (Request));
      Setup_Request := From_JSON (Request_JSON);
      
      Self.Database.Add_Chart_Setup
        (To_String (Setup_Request.Chart_Name),
         Setup_Request.Start_Time,
         Setup_Request.End_Time);
         
      Alog.Active_Logger.Log
        (Level => Alog.Info,
         Msg   => "Added setup for chart: " & To_String (Setup_Request.Chart_Name));
         
      return Create_JSON_Response
        (Content => "{""message"": ""Chart setup saved successfully""}");
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to save chart setup: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to save chart setup",
            Status  => 500);
   end Add_Chart_Setup;
   
   function Add_Annotation
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Request_JSON : JSON_Value;
      Annotation_Req : Annotation_Request;
   begin
      Request_JSON := Parse_JSON_String (AWS.Status.Payload (Request));
      Annotation_Req := From_JSON (Request_JSON);
      
      Self.Database.Add_Annotation
        (Annotation_Req.Data_Point_ID,
         To_String (Annotation_Req.Annotation));
         
      Alog.Active_Logger.Log
        (Level => Alog.Debug,
         Msg   => "Added annotation to point: " & Annotation_Req.Data_Point_ID'Image);
         
      return Create_JSON_Response
        (Content => "{""message"": ""Annotation added successfully""}");
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to add annotation: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to add annotation",
            Status  => 500);
   end Add_Annotation;
   
   function Get_Annotation
     (Self    : Server_Type;
      Request : AWS.Status.Data) return AWS.Response.Data 
   is
      Params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Data_Point_ID : constant Integer := Integer'Value (Get_Required_Param (Params, "dataPointId"));
      Annotation : String;
   begin
      Annotation := Self.Database.Get_Annotation (Data_Point_ID);
      return Create_JSON_Response
        (Content => "{""annotation"": """ & Annotation & """}");
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Failed to get annotation: " & 
                    Ada.Exceptions.Exception_Message (E));
         return Create_Error_Response
           (Message => "Failed to get annotation",
            Status  => 500);
   end Get_Annotation;
   
end Server;
