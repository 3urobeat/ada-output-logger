-- File: helpers.ads
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-07-28 21:14:06
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Text_IO;
with Colors_Collection;
with Construct;
with File_Output;

use Ada.Text_IO;
use Colors_Collection;
use Construct;


package Helpers is

   -- Internal: Expands Construct_Message_Prefix call to construct a log message prefix
   -- @param Color Optional: Color code for log level parameter
   -- @param Lvl Log level of this message
   -- @param SRC Optional: User provided name of the file this log message originates from
   -- @param ND Optional: No-Date - User provided setting if date should not include a timestamp
   -- @return Returns the formatted message prefix string
   function Get_Prefix(Color : String := ""; Lvl : String; SRC : String := ""; ND : Boolean := False) return String;

   -- Internal: Constructs a String of whitespaces to concat to a log message if the last message was longer than the current message is. This prevents ghost chars from messages marked as Rm.
   -- @param Current_Message_Length Length of the current message
   -- @param Last_Message_Length Length of the last message
   -- @return Returns string to append to the current message
   function Get_Trailing_Whitespaces(Current_Message_Length : Natural; Last_Message_Length : Natural) return String;

   -- Internal: Cuts a String to the width of the current terminal and returns it
   -- @param str String to cut
   -- @param Subtract Optional: Length to subtract from terminal width (for example an already printed string)
   -- @return Returns the cut string
   function Cut_To_Terminal_Width(str : String; Subtract : Natural := 0) return String;

private

end Helpers;
