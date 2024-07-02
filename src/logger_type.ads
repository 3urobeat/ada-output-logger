-- File: logger_type.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-02 20:28:58
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Text_IO;
with Colors;
with Construct;

use Ada.Text_IO;


package Logger_Type is

   -- Dummy type to allow functions returning a reference to "itself"-ish
   type Logger_Dummy is tagged record
      null;
   end record;


   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   function Log(this : Logger_Dummy; str : String) return Logger_Dummy;

   -- Logs a message to stdout with 'INFO' prefix
   function Info(this : Logger_Dummy; str : String) return Logger_Dummy;

   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : Logger_Dummy; str : String) return Logger_Dummy;

   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : Logger_Dummy; str : String) return Logger_Dummy;

   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : Logger_Dummy; str : String) return Logger_Dummy;

   -- Logs a newline to stdout
   function Nl(this : Logger_Dummy) return Logger_Dummy;

   -- Ends the message. This is a required dummy function as Ada forces us to process return values, which we don't want when being done calling Logger functions
   procedure EoL(this : Logger_Dummy) is null;


   -- Create instance of Logger_Dummy for everyone to use
   Logger : Logger_Dummy;

private

end Logger_Type;
