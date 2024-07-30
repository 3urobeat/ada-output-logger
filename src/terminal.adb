-- File: terminal.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-26 20:44:52
-- Author: 3urobeat
--
-- Last Modified: 2024-07-26 21:02:44
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


package body Terminal is

   -- Gets the width (columns) of the current terminal (supports only Linux atm)
   function Get_Terminal_Width return Positive is
      Window_Size : Terminal_Dimensions_Type;
      Std_Input_FD : constant C.int := 0;  -- File Descriptor for stdin
   begin

      -- Attempt to fill Window_Size with data from Ioctl. Throw error if Ioctl did not succeed.
      if Ioctl(Std_Input_FD, TIOCGWINSZ, Window_Size'Address) /= 0 then
         raise Program_Error;
      end if;

      return Positive(Window_Size.Cols);

   end Get_Terminal_Width;

end Terminal;
