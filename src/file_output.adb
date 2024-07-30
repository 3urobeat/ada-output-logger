-- File: file_output.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:57:26
-- Author: 3urobeat
--
-- Last Modified: 2024-07-06 16:25:40
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


package body File_Output is

   -- Internal: Creates (if not already done), opens file at path and populates Output_File
   procedure Open_File(path : String) is
   begin
      if Ada.Directories.Exists(path) = False then
         Create(Output_File, Append_File, path);
      else
         Open(Output_File, Append_File, path);
      end if;
   end Open_File;


   -- Internal: Calls Open_File if necessary and writes str to Output_File. If path is empty, the call will be ignored.
   procedure Print_To_File(path : String; str : String) is
   begin                                                       -- TODO: This function can surely be optimized
      if path'Length = 0 then
         return;
      end if;

      if Is_Open(Output_File) = False then
         Open_File(path);
      end if;

      Put(Output_File, str);
   end Print_To_File;

end File_Output;
