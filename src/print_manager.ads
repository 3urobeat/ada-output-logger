-- File: print_manager.ads
-- Project: ada-output-logger
-- Created Date: 2024-08-03 16:56:03
-- Author: 3urobeat
--
-- Last Modified: 2024-12-01 12:47:22
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Progress;
with Terminal;

use Ada.Text_IO;
use Ada.Characters.Latin_1;


package Print_Manager is

   -- Type of events the Print_Manager supports
   type Print_Event_Type is ( Animation_Create, Animation_Update, Animation_Remove, Progress_Create, Progress_Update, Progress_Remove, Read_Input_Start, Read_Input_End, Message, Ctrl_Char, Finalize );

   -- TODO: Storing this here outside of any record sucks so bad
   Current_Progress_Bar : access Progress.Internal_Progress_Type := null;
   Pending_Newline : Boolean := False;


   -- Clears the contents of the current stdout line
   procedure Clear_Line;

   -- Moves the cursor to a line relative from its current position
   procedure Move_Cursor(Relative : Short_Integer);

   -- Manages cursor movement and logs a string to stdout
   -- @param Event How this message should be handled
   -- @param Str The String to log
   procedure Print(Event : Print_Event_Type; Str : String);

   -- Registers a newline, fulfilled by the next Print call
   procedure Newline;

   -- Locks output to stdout and queues new messages instead
   procedure Lock_Stdout;

   -- Unlocks output to stdout and processes all queued messages
   procedure Unlock_Stdout;

private

   type String_Access is access String;

   -- Create a container to store queued log messages in
   package Log_Queue_Container is new
      Ada.Containers.Vectors(
         Index_Type => Natural,
         Element_Type => String_Access
      );

   Log_Queue : Log_Queue_Container.Vector;

   -- Create a container to cache the last 3 stdout lines in
   type Line_Cache_Type is array (Natural range 0 .. 2) of String_Access;

   Line_Cache : Line_Cache_Type := (1 => new String'(""), others => null);


   -- Internal: Whether new messages should currently be hold back from stdout (for example while Read_Input is active)
   Stdout_Is_Locked : Boolean := False;

end Print_Manager;
