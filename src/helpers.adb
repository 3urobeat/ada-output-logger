-- File: helpers.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-07-05 15:23:00
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

end Helpers;
