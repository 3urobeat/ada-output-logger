-- File: helpers.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-07-06 15:47:48
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


package body Helpers is

   -- Internal: Expands Construct_Message_Prefix call to construct a log message prefix
   function Get_Prefix(Color : String := ""; Lvl : String; SRC : String := ""; ND : Boolean := False) return String is
   begin
      -- This function call is way too overcomplicated
      return Construct_Message_Prefix(
         Color & Lvl & (if Color'Length > 0 then Colors.reset else ""),
         (if SRC'Length > 0 then                                              -- Only add color to SRC param if SRC was provided
            (Color & SRC & (if Color'Length > 0 then Colors.reset else ""))
         else
            ""
         ),
         ND,
         Color'Length = 0 -- Do not color date if log level is not colored (for example for file output)
      );
   end Get_Prefix;


   -- Internal: Constructs a String of whitespaces to concat to a log message if the last message was longer than the current message is. This prevents ghost chars from messages marked as Rm.
   function Get_Trailing_Whitespaces(Current_Message_Length : Natural; Last_Message_Length : Natural) return String is
      Diff : Integer := Integer'(Last_Message_Length) - Integer'(Current_Message_Length); -- Cast to Integers because a negative result is possible
   begin
      if Diff > 0 then
         declare
            Diff_Str : String(1 .. Diff) := (others => ' '); -- Create string of whitespaces if new message is < previous message
         begin
            return Diff_Str;
         end;
      else
         return ""; -- Return empty string if new message is >= previous message
      end if;
   end Get_Trailing_Whitespaces;

end Helpers;
