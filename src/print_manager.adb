-- File: print_manager.adb
-- Project: ada-output-logger
-- Created Date: 2024-08-03 16:56:03
-- Author: 3urobeat
--
-- Last Modified: 2024-08-23 19:54:23
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


package body Print_Manager is

   -- Manages cursor movement and logs a string to stdout
   procedure Print(Event : Print_Event_Type; Str : String) is
   begin

      case Event is
         when Animation_Create =>
            Put(Str);

         when Animation_Update =>
            Put(Str);

         when Animation_Remove =>
            null;

         when Read_Input_Start =>
            Put(Str);

         when Read_Input_End =>
            null;

         when Message =>
            if Stdout_Is_Locked then
               Log_Queue.Append(new String'(Str));
            else
               Put(Str);
            end if;

         when Finalize =>
            null;

         when others =>
            null;

      end case;

   end Print;


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
         Print(Message, Str.all);
      end loop;

      Log_Queue.Clear;
   end Unlock_Stdout;

end Print_Manager;
