-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-06 15:27:19
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


package body Logger_Type is


   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   function Log(this : access Logger_Dummy; STR : String) return access Logger_Dummy is
   begin
      File_Output.Print_To_File(STR);
      Internal_Log(STR);
      this.Current_Message_Length := this.Current_Message_Length + STR'Length;

      return this;
   end Log;


   -- Logs a message to stdout with 'INFO' prefix
   function Info(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      Internal_Prefixed_Log(this.Current_Message_Length, "INFO", Colors.brfgcyan, STR, SRC, ND);

      return this;
   end Info;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      Internal_Prefixed_Log(this.Current_Message_Length, "DEBUG", Colors.brfgcyan & Colors.background, STR, SRC, ND);

      return this;
   end Debug;


   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      Internal_Prefixed_Log(this.Current_Message_Length, "WARN", Colors.fgred, STR, SRC, ND);

      return this;
   end Warn;


   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      Internal_Prefixed_Log(this.Current_Message_Length, "ERROR", Colors.fgred & Colors.background, Colors.fgred & STR & Colors.reset, SRC, ND);

      return this;
   end Error;


   -- Logs a newline to stdout
   function Nl(this : access Logger_Dummy) return access Logger_Dummy is
   begin
      -- TODO: Check if whitespaces need to be added because previous message was marked as Rm

      File_Output.Print_To_File("" & Ada.Characters.Latin_1.LF);
      New_Line;

      return this;
   end Nl;


   -- Marks this message to be overwritten by the next logger call and ends the message
   procedure RmEoL(this : access Logger_Dummy) is
   begin
      -- TODO: Cut this message to terminal width to prevent message not being able to get cleared. This is a problem because the current message could already have overflown one line
      -- TODO: Check if whitespaces need to be added because previous message was marked as Rm

      File_Output.Print_To_File("" & Ada.Characters.Latin_1.LF); -- Print a newline to the output file as nothing can be overwritten there
      Internal_Log("" & Ada.Characters.Latin_1.CR);              -- Print a carriage return to stdout (so the next msg overwrites this one)

      -- Save size so the next message can overwrite everything we've printed to avoid ghost chars
      this.Overwrite_Chars_Amount := this.Current_Message_Length;
      this.Current_Message_Length := 0;
   end RmEoL;


   -- Ends the message. This is a required dummy function as Ada forces us to process return values, which we don't want when being done calling Logger functions
   procedure EoL(this : access Logger_Dummy) is
   begin
      -- TODO: Check if whitespaces need to be added because previous message was marked as Rm

      -- Reset size tracker. Since this message was not marked to be removed, we don't need to keep it
      this.Current_Message_Length := 0;
   end EoL;



   -- Internal: Logs a message to stdout without appending to output file (as this is already handled by the external functions)
   procedure Internal_Log(str : String) is
   begin
      Put(str);
   end Internal_Log;


   -- Internal: Constructs the actual message and logs it to file & stdout
   procedure Internal_Prefixed_Log(Current_Message_Length : in out Natural; Log_Lvl : String; Color : String; STR : String; SRC : String := ""; ND : Boolean := False) is
      Msg_No_Color : String := Helpers.Get_Prefix("", Log_Lvl, SRC, ND) & str;
   begin

      -- Construct message without colors for output file
      File_Output.Print_To_File(Msg_No_Color);

      -- Add size in case this message shall be overwritten later on
      Current_Message_Length := Current_Message_Length + Msg_No_Color'Length;

      -- Construct message with colors and let the internal plain logger function log it
      Internal_Log(Helpers.Get_Prefix(Color, Log_Lvl, SRC, ND) & str);

   end Internal_Prefixed_Log;

end Logger_Type;
