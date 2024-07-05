-- File: logger_type.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-05 15:21:00
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
with File_Output;
with Helpers;

use Ada.Text_IO;
use Construct;


package Logger_Type is

   -- Dummy type to allow functions returning a reference to "itself"-ish
   type Logger_Dummy is tagged record
      null;
   end record;


   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Log(this : Logger_Dummy; STR : String) return Logger_Dummy;

   -- Logs a message to stdout with 'INFO' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @param RM Optional: Remove - Set to true if the next message should overwrite this one. Make sure to not call Nl()!
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Info(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy;

   -- Logs a message to stdout with 'DEBUG' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @param RM Optional: Remove - Set to true if the next message should overwrite this one. Make sure to not call Nl()!
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Debug(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy;

   -- Logs a message to stdout with 'WARN' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @param RM Optional: Remove - Set to true if the next message should overwrite this one. Make sure to not call Nl()!
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Warn(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy;

   -- Logs a message to stdout with 'ERROR' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @param RM Optional: Remove - Set to true if the next message should overwrite this one. Make sure to not call Nl()!
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Error(this : Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False) return Logger_Dummy;

   -- Logs a newline to stdout
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Nl(this : Logger_Dummy) return Logger_Dummy;

   -- Ends the message. This is a required dummy function as Ada forces us to process return values, which we don't want when being done calling Logger functions
   -- @param this Instance of Logger, automatically provided when using dot notation
   procedure EoL(this : Logger_Dummy) is null;


   -- Create instance of Logger_Dummy for everyone to use
   Logger : Logger_Dummy;

private

   -- Internal: Logs a message to stdout without appending to output file (as this is already handled by the external functions)
   -- @param str User provided message to log
   procedure Internal_Log(str : String);

   -- Internal: Constructs the actual message and logs it to file & stdout
   procedure Internal_Prefixed_Log(Log_Lvl : String; Color : String; STR : String; SRC : String := ""; ND : Boolean := False; RM : Boolean := False);

end Logger_Type;
