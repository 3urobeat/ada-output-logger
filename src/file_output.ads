-- File: file_output.ads
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:57:26
-- Author: 3urobeat
--
-- Last Modified: 2024-08-01 19:13:07
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Text_IO;
with Ada.Directories;

use Ada.Text_IO;


package File_Output is

   -- Internal: Calls Open_File if necessary and writes str to Output_File. If path is empty, the call will be ignored.
   procedure Print_To_File(path : String; str : String);

private

   -- Reference to output file opened by Open_File
   Output_File : File_Type;

   -- Internal: Creates (if not already done), opens file at path and populates Output_File
   procedure Open_File(path : String);

   -- Internal: Removes escape chars (e.g. color codes) from a String
   function Remove_Escape_Chars(Input : String) return String;

end File_Output;
