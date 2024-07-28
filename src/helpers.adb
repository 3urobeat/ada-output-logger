-- File: helpers.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-07-28 15:11:52
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Colors_Collection;
with Terminal;

use Colors_Collection;


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


   -- Internal: Logs a message to stdout without appending to output file (as this is already handled by the external functions)
   procedure Internal_Log(Str : String; Current_Message_Length : in out Natural; Cut : Boolean := False) is
   begin
      -- Check if message needs to be cut
      if Cut then
         declare
            Cut_Str : String := Cut_To_Terminal_Width(Str, Current_Message_Length);
         begin
            Put(Cut_Str);
            Current_Message_Length := Current_Message_Length + Cut_Str'Length; -- TODO: This is not entirely accurate because it counts color codes
         end;
      else
         Put(Str);
         Current_Message_Length := Current_Message_Length + Str'Length; -- TODO: This is not entirely accurate because it counts color codes
      end if;

      -- Always append Color Reset to avoid colors bleeding into the next element
      Put(Colors.reset);
   end Internal_Log;


   -- Internal: Constructs the actual message and logs it to file & stdout
   procedure Internal_Prefixed_Log(Output_File_Path : String; Current_Message_Length : in out Natural; Log_Lvl : String; Color : String; STR : String; SRC : String := ""; ND : Boolean := False; Cut : Boolean := False) is
      Msg_No_Color : String := Get_Prefix("", Log_Lvl, SRC, ND) & str;
   begin

      -- Construct message without colors for output file
      File_Output.Print_To_File(Output_File_Path, Msg_No_Color);

      -- Construct message with colors and let the internal plain logger function log it
      Internal_Log(
         str => Get_Prefix(Color, Log_Lvl, SRC, ND) & str,
         Current_Message_Length => Current_Message_Length,
         Cut => Cut
      );

   end Internal_Prefixed_Log;

end Helpers;
