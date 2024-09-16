-- File: file_output.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:57:26
-- Author: 3urobeat
--
-- Last Modified: 2024-09-16 18:09:50
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


package body File_Output is

   -- Internal: Calls Open_File if necessary and writes str to Output_File. If path is empty, the call will be ignored.
   procedure Print_To_File(handle : access File_Type; str : String) is
   begin                                                       -- TODO: This function can surely be optimized
      -- Remove colors from str and print to handle, as long as handle is not null
      if handle /= null then
         Put(handle.all, Remove_Escape_Chars(str));
      end if;
   end Print_To_File;


   -- Internal: Removes escape chars (e.g. color codes) from a String
   function Remove_Escape_Chars(Input : String) return String is
      Output : String (1 .. Input'Length) := (others => ' ');
      Output_Len : Natural := 0;
      Skip : Boolean := False;
   begin
      for I in Input'Range loop

         if Skip then
            if Input(I) in 'A' .. 'Z' or else Input(I) in 'a' .. 'z' then
               Skip := False;
            end if;
         else
            if Input(I) = ASCII.ESC then
               Skip := True;
            else
               Output_Len         := Output_Len + 1;
               Output(Output_Len) := Input(I);
            end if;
         end if;

      end loop;

      return Output(1 .. Output_Len);
   end Remove_Escape_Chars;

end File_Output;
