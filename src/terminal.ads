-- File: terminal.ads
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


with Interfaces.C;
with System;


package Terminal is

   -- Gets the width (columns) of the current terminal (supports only Linux atm)
   -- @return Returns the amount of columns of the current terminal
   function Get_Terminal_Width return Positive;

private

   -- TODO: Can this be shortened?
   -- TODO: Add windows support and surround it with pre-processor macros

   -- Get datatypes from C to pass to Ioctl
   package C renames Interfaces.C;
   use type C.int;
   use type C.short;

   -- Stores data returned by Ioctl
   type Terminal_Dimensions_Type is record
      Rows : C.short;
      Cols : C.short;
      Xpixel : C.short;
      Ypixel : C.short;
   end record;
   pragma Convention (C, Terminal_Dimensions_Type);

   TIOCGWINSZ : constant C.int := 16#5413#;

   function Ioctl (Fd : C.int; Request : C.int; Arg : System.Address) return C.int;
   pragma Import (C, Ioctl, "ioctl");

end Terminal;
