-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-06-30 16:56:26
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


package body Logger_Type is

   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   function Log(this : Logger_Dummy; str : String) return Logger_Dummy is
   begin
      Put(str);

      return this;
   end Log;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Info(this : Logger_Dummy; str : String) return Logger_Dummy is
   begin
      return this.Log("[INFO] " & str);
   end Info;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : Logger_Dummy; str : String) return Logger_Dummy is
   begin
      return this.Log("[DEBUG] " & str);
   end Debug;


   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : Logger_Dummy; str : String) return Logger_Dummy is
   begin
      return this.Log("[WARN] " & str);
   end Warn;


   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : Logger_Dummy; str : String) return Logger_Dummy is
   begin
      return this.Log("[ERROR] " & str);
   end Error;


   -- Logs a newline to stdout
   function Nl(this : Logger_Dummy) return Logger_Dummy is
   begin
      New_Line;

      return this;
   end Nl;

end Logger_Type;
