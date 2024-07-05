-- File: file_output.ads
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:57:26
-- Author: 3urobeat
--
-- Last Modified: 2024-07-05 15:11:50
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Text_IO;
with Ada.Directories;

use Ada.Text_IO;


package File_Output is

   procedure Open_File(path : String);

   procedure Print_To_File(str : String);

private

   Output_File : File_Type;

end File_Output;
