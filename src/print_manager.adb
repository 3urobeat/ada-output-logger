-- File: print_manager.adb
-- Project: ada-output-logger
-- Created Date: 2024-08-03 16:56:03
-- Author: 3urobeat
--
-- Last Modified: 2024-11-29 11:49:47
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


package body Print_Manager is

   -- Clears the contents of the current stdout line
   procedure Clear_Line is                            -- TODO: This function could theoretically be optimized by using ANSI control chars instead? (I guess?)
      Str : String(1 .. Terminal.Get_Terminal_Width) := (others => ' ');
   begin
      Put(Str);
   end Clear_Line;


   -- Moves the cursor to a line relative from its current position
   procedure Move_Cursor(Relative : Short_Integer) is
      Relative_Str : String := Ada.Strings.Fixed.Trim(Short_Integer'Image(abs Relative), Ada.Strings.Left);
   begin
      if Relative < 0 then
         Put(ESC & "[" & Relative_Str & "A");
      else
         Put(ESC & "[" & Relative_Str & "B");
      end if;
   end Move_Cursor;


   -- Manages cursor movement and logs a string to stdout
   procedure Print(Event : Print_Event_Type; Str : String) is

      -- Helper function to handle appending to queue or logging directly
      procedure Print_When_Unlocked(Str : String) is
      begin
         if Stdout_Is_Locked then
            Log_Queue.Append (new String'(Str));
         else
            Put(Standard_Output, Str);   -- Specifying Standard_Output can fix an exception when calling Ada from C
         end if;
      end Print_When_Unlocked;

      -- Processes a pending newline if necessary on certain events
      procedure Process_Pending_Newline is
      begin
         if Pending_Newline then
            Pending_Newline := False;
            Print(Event => Message, Str => "" & LF); -- Using Print() here to avoid having to do Cursor Management here again
         end if;
      end Process_Pending_Newline;

   begin


      case Event is
         when Animation_Create =>
            Process_Pending_Newline;
            Print_When_Unlocked(Str);

         when Animation_Update =>
            Print_When_Unlocked(Str);

         when Animation_Remove =>
            null;

         when Progress_Create =>
            -- TODO: Cursor handling
            Print_When_Unlocked(Str);

         when Progress_Update =>
            null;

         when Progress_Remove =>
            null;

         when Read_Input_Start =>
            Process_Pending_Newline;

            Put(Standard_Output, Str);

         when Read_Input_End =>
            null;
         when Message =>
            Process_Pending_Newline;

            Print_When_Unlocked(Str);

         when Ctrl_Char =>
            Print_When_Unlocked(Str);
         when Finalize =>
            Unlock_Stdout;
            Process_Pending_Newline;
            Put(Standard_Output, Str);

         when others =>
            null;

      end case;

   end Print;

   -- Registers a newline, fulfilled by the next Print call
   procedure Newline is
   begin
      -- If Newline was called multiple times between a Print call we need to fulfill ourselves first
      if Pending_Newline then
         Print(Event => Message, Str => ""); -- Empty String because Print checks for Pending_Newline and prints LF itself
      end if;

      Pending_Newline := True;
   end Newline;

   -- Locks output to stdout and queues new messages instead
   procedure Lock_Stdout is
   begin
      Stdout_Is_Locked := True;
   end Lock_Stdout;


   -- Unlocks output to stdout and processes all queued messages
   procedure Unlock_Stdout is
   begin
      Stdout_Is_Locked := False;

      -- Iterate through all queued messages and log them
      for Str of Log_Queue loop
         Print(Event => Message,
               Str => Str.all);
      end loop;

      Log_Queue.Clear;
   end Unlock_Stdout;

end Print_Manager;
