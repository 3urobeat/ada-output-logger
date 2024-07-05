-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-05 19:31:08
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


package body Logger_Type is


   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   function Log(this : Logger_Dummy; STR : String) return Logger_Dummy is
   begin
      File_Output.Print_To_File(STR);
      Internal_Log(STR);

      return this;
   end Log;


   -- Logs a message to stdout with 'INFO' prefix
   function Info(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return Logger_Dummy is
   begin
      Internal_Prefixed_Log("INFO", Colors.brfgcyan, STR, SRC, ND);

      return this;
   end Info;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return Logger_Dummy is
   begin
      Internal_Prefixed_Log("DEBUG", Colors.brfgcyan & Colors.background, STR, SRC, ND);

      return this;
   end Debug;


   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return Logger_Dummy is
   begin
      Internal_Prefixed_Log("WARN", Colors.fgred, STR, SRC, ND);

      return this;
   end Warn;


   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return Logger_Dummy is
   begin
      Internal_Prefixed_Log("ERROR", Colors.fgred & Colors.background, Colors.fgred & STR & Colors.reset, SRC, ND);

      return this;
   end Error;


   -- Logs a newline to stdout
   function Nl(this : Logger_Dummy) return Logger_Dummy is
   begin
      File_Output.Print_To_File("" & Ada.Characters.Latin_1.LF);
      New_Line;

      return this;
   end Nl;


   -- Marks this message to be overwritten by the next logger call and ends the message
   procedure RmEoL(this : Logger_Dummy) is
   begin
      File_Output.Print_To_File("" & Ada.Characters.Latin_1.LF); -- Print a newline to the output file as nothing can be overwritten there
      Internal_Log("" & Ada.Characters.Latin_1.CR);                   -- Print a carriage return to stdout (so the next msg overwrites this one)
   end RmEoL;




   -- Internal: Logs a message to stdout without appending to output file (as this is already handled by the external functions)
   procedure Internal_Log(str : String) is
   begin
      Put(str);
   end Internal_Log;


   -- Internal: Constructs the actual message and logs it to file & stdout
   procedure Internal_Prefixed_Log(Log_Lvl : String; Color : String; STR : String; SRC : String := ""; ND : Boolean := False) is
   begin

      -- Construct message without colors for output file
      File_Output.Print_To_File(Helpers.Get_Prefix("", Log_Lvl, SRC, ND) & str);

      -- Construct message with colors and let the internal plain logger function log it
      Internal_Log(Helpers.Get_Prefix(Color, Log_Lvl, SRC, ND) & str);

   end Internal_Prefixed_Log;

end Logger_Type;
