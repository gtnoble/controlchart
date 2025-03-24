with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Interrupts.Names;
with Ada.Task_Identification;
with Parse_Args;
with Server;
with Alog.Logger;
with Alog.Facilities.File_Descriptor;
with Alog.Active_Logger;
with Ada.Text_IO.File_Descriptors;
with Ada.Characters.Handling;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Alog.Transforms.Timestamp;

procedure Control_Chart_Backend is
   -- Application constants
   Version : constant String := "1.0.0";
   
   -- Logging configuration
   Logger_Transform : constant Alog.Transforms.Timestamp.Instance :=
     Alog.Transforms.Timestamp.Create
       (Format    => "[%Y-%m-%d %H:%M:%S] %C %M",
        Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
   
   function Get_Log_Level (Level : String) return Alog.Level_Type is
      Level_Upper : constant String := Ada.Characters.Handling.To_Upper (Level);
   begin
      if Level_Upper = "DEBUG" then
         return Alog.Debug;
      elsif Level_Upper = "INFO" then
         return Alog.Info;
      elsif Level_Upper = "WARN" then
         return Alog.Warning;
      elsif Level_Upper = "ERROR" then
         return Alog.Error;
      else
         return Alog.Info;
      end if;
   end Get_Log_Level;
   -- Set up logging facilities
   Logger : Alog.Logger.Logger_Type;
   
   -- File logging facility
   FD_Facility : constant Alog.Facilities.File_Descriptor.Instance :=
     Alog.Facilities.File_Descriptor.Create
       (Name => "File_Facility",
        Path => "control_chart.log");
        
   -- Console logging facility (stderr)
   Console_Facility : constant Alog.Facilities.File_Descriptor.Instance :=
     Alog.Facilities.File_Descriptor.Create
       (Name => "Console_Facility",
        FD => Ada.Text_IO.File_Descriptors.Standerr);
   -- Protected object for signal handling
   protected Shutdown_Signal is
      procedure Stop;
      function Is_Stopped return Boolean;
   private
      Stopped : Boolean := False;
   end Shutdown_Signal;
   
   protected body Shutdown_Signal is
      procedure Stop is
      begin
         Stopped := True;
      end Stop;
      
      function Is_Stopped return Boolean is
      begin
         return Stopped;
      end Is_Stopped;
   end Shutdown_Signal;
   
   -- Signal handler for Ctrl+C
   protected Interrupt_Handler is
      procedure Handle_Interrupt;
      pragma Interrupt_Handler (Handle_Interrupt);
      pragma Attach_Handler (Handle_Interrupt, Ada.Interrupts.Names.SIGINT);
   end Interrupt_Handler;
   
   protected body Interrupt_Handler is
      procedure Handle_Interrupt is
      begin
         Alog.Active_Logger.Log
           (Level => Alog.Info,
            Msg   => "Received interrupt signal, initiating shutdown...");
         Shutdown_Signal.Stop;
      end Handle_Interrupt;
   end Interrupt_Handler;
   
   task Signal_Handler is
      entry Start;
   end Signal_Handler;
   
   task body Signal_Handler is
   begin
      accept Start;
      
      -- Main signal handling loop
      loop
         exit when Shutdown_Signal.Is_Stopped;
         delay 0.1;  -- Small delay to reduce CPU usage
      end loop;
   exception
      when E : others =>
         Alog.Active_Logger.Log
           (Level => Alog.Error,
            Msg   => "Signal handler error: " & Ada.Exceptions.Exception_Message (E));
   end Signal_Handler;
   
   package CLI renames Parse_Args;
   
   procedure Print_Help is
   begin
      Ada.Text_IO.Put_Line ("Usage: control_chart_backend [OPTIONS]");
      Ada.Text_IO.Put_Line ("Options:");
      Ada.Text_IO.Put_Line ("  -d, --database PATH  Path to SQLite database file (required)");
      Ada.Text_IO.Put_Line ("  -h, --host HOST     Host to bind to (default: localhost)");
      Ada.Text_IO.Put_Line ("  -p, --port PORT     Port to listen on (default: 3000)");
      Ada.Text_IO.Put_Line ("  -l, --log-level LVL Log level (debug|info|warn|error, default: info)");
      Ada.Text_IO.Put_Line ("  --help             Show this help message");
   end Print_Help;
   
   Parser : CLI.Parser;
   Chart_Server : Server.Server_Type;
begin
   -- Set up command line argument parser
   CLI.Add_Option (Parser, "-d", "--database",
                  "Path to SQLite database file",
                  Required => True);
   CLI.Add_Option (Parser, "-h", "--host",
                  "Host to bind to",
                  Default => "localhost");
   CLI.Add_Option (Parser, "-p", "--port",
                  "Port to listen on",
                  Default => "3000");
   CLI.Add_Option (Parser, "-l", "--log-level",
                  "Log level (debug|info|warn|error)",
                  Default => "info");
   CLI.Add_Option (Parser, "", "--help",
                  "Show help message");
   
   -- Parse command line arguments
   if not CLI.Parse (Parser) then
      Print_Help;
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   
   -- Check for help flag
   if CLI.Get (Parser, "help") /= "" then
      Print_Help;
      return;
   end if;
   
   -- Get required arguments
   declare
      Database_Path : constant String := CLI.Get (Parser, "database");
      Host : constant String := CLI.Get (Parser, "host");
      Port : constant Positive := Positive'Value (CLI.Get (Parser, "port"));
   begin
      -- Initialize logging
      Alog.Active_Logger.Log
        (Level => Alog.Info,
         Msg   => "Control Chart Backend v" & Version & " starting up");
         
      -- Configure logging facilities
      declare
         Log_Level : constant Alog.Level_Type := 
           Get_Log_Level (CLI.Get (Parser, "log-level"));
      begin
         -- Set up logging format and level for both facilities
         FD_Facility.Set_Transform (Logger_Transform);
         Console_Facility.Set_Transform (Logger_Transform);
         FD_Facility.Set_Log_Level_Threshold (Log_Level);
         Console_Facility.Set_Log_Level_Threshold (Log_Level);
         Logger.Attach_Facility (FD_Facility);
         Logger.Attach_Facility (Console_Facility);
         Alog.Active_Logger.Set_Logger (Logger);
         
         Alog.Active_Logger.Log
           (Level => Alog.Debug,
            Msg   => "Initialized logging with level: " & Log_Level'Image);
      end;
      
      -- Server initialization
      Alog.Active_Logger.Log (Level => Alog.Info,
                             Msg   => "Starting Control Chart Server...");
      Alog.Active_Logger.Log (Level => Alog.Info,
                             Msg   => "Database: " & Database_Path);
      Alog.Active_Logger.Log (Level => Alog.Info,
                             Msg   => "Host: " & Host & ":" & Port'Image);
      
      Chart_Server := Server.Create
        (Database_Path => Database_Path,
         Host         => Host,
         Port         => Port);
         
      Chart_Server.Start;
      
      Alog.Active_Logger.Log (Level => Alog.Info,
                             Msg   => "Server started. Press Ctrl+C to stop.");
      
      -- Signal handling setup
      Signal_Handler.Start;
      
      -- Main server loop
      while not Shutdown_Signal.Is_Stopped loop
         delay 1.0;
      end loop;
      
      -- Clean shutdown
      Chart_Server.Stop;
      Alog.Active_Logger.Log (Level => Alog.Info,
                             Msg   => "Server stopped.");
      
   exception
      when E : others =>
      -- Initialization error handling
      Alog.Active_Logger.Log (Level => Alog.Error,
                             Msg   => "Error: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);
      Shutdown_Signal.Stop;  -- Signal the handler to stop
   end;
   
exception
   when E : others =>
      Alog.Active_Logger.Log (Level => Alog.Error,
                             Msg   => "Error: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Control_Chart_Backend;
