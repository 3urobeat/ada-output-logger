-- File: helpers.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-08-01 19:37:35
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Colors_Collection;
with Terminal;

use Colors_Collection;


package body Helpers is

   -- Internal: Expands Construct_Message_Prefix call to construct a log message prefix
   function Get_Prefix(Color : String := ""; Lvl : String; SRC : String := ""; ND : Boolean := False) return String is
   begin
      -- This function call is way too overcomplicated
      return Construct_Message_Prefix(
         Color & Lvl & (if Color'Length > 0 then Colors.Reset else ""),
         (if SRC'Length > 0 then                                              -- Only add color to SRC param if SRC was provided
            (Color & SRC & (if Color'Length > 0 then Colors.Reset else ""))
         else
            ""
         ),
         ND,
         Color'Length = 0 -- Do not color date if log level is not colored
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


   -- Internal: Cuts a String to the width of the current terminal and returns it
   function Cut_To_Terminal_Width(str : String; Subtract : Natural := 0) return String is
      Width : Natural := Terminal.Get_Terminal_Width; -- TODO: Exception handling
   begin
      Width := Natural'Max(Width - Subtract, 0); -- Subtract what is already printed from how much we can print, however make sure we aren't going negative

      if str'Length > Width then                 -- Make sure width is < length to avoid a constraint_error
         return str(str'First .. str'First + Width - 1);
      else
         return str;
      end if;
   end Cut_To_Terminal_Width;

end Helpers;
