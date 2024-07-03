-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-03 18:53:35
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
      Put(str);

      return this;
   end Log;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Info(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy is
   begin
      return this.Log(
         Helpers.Get_Prefix(Colors.brfgcyan, "INFO", SRC, ND)
         & str
         & Construct_Message_Suffix(RM)
      );
   end Info;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy is
   begin
      return this.Log(
         Helpers.Get_Prefix(Colors.brfgcyan & Colors.background, "DEBUG", SRC, ND)
         & str
         & Construct_Message_Suffix(RM)
      );
   end Debug;


   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy is
   begin
      return this.Log(
         Helpers.Get_Prefix(Colors.fgred, "WARN", SRC, ND)
         & str
         & Construct_Message_Suffix(RM)
      );
   end Warn;


   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy is
   begin
      return this.Log(
         Helpers.Get_Prefix(Colors.fgred & Colors.background, "ERROR", SRC, ND)
         & str
         & Construct_Message_Suffix(RM)
      );
   end Error;


   -- Logs a newline to stdout
   function Nl(this : Logger_Dummy) return Logger_Dummy is
   begin
      New_Line;

      return this;
   end Nl;

end Logger_Type;
