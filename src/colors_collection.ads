-- File: colors_collection.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 15:27:41
-- Author: 3urobeat
--
-- Last Modified: 2024-08-01 19:37:35
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Characters.Latin_1; -- Used for escape character below

use Ada.Characters.Latin_1;


package Colors_Collection is

   type Colors_Type is record
      -- Cursor Control codes
      Hide_Cursor    : String(1 .. 6) := ESC & "[?25l";
      Show_Cursor    : String(1 .. 6) := ESC & "[?25h";

      -- General Control codes
      Reset          : String(1 .. 4) := ESC & "[0m";
      Bold           : String(1 .. 4) := ESC & "[1m";
      Dim            : String(1 .. 4) := ESC & "[2m";
      Italic         : String(1 .. 4) := ESC & "[3m";
      Underscore     : String(1 .. 4) := ESC & "[4m";
      Blink          : String(1 .. 4) := ESC & "[5m";
      Background     : String(1 .. 4) := ESC & "[7m";
      Hidden         : String(1 .. 4) := ESC & "[8m";
      Crossed        : String(1 .. 4) := ESC & "[9m";

      -- Foreground colors
      Fg_Black       : String(1 .. 5) := ESC & "[30m";
      Fg_Red         : String(1 .. 5) := ESC & "[31m";
      Fg_Green       : String(1 .. 5) := ESC & "[32m";
      Fg_Yellow      : String(1 .. 5) := ESC & "[33m";
      Fg_Blue        : String(1 .. 5) := ESC & "[34m";
      Fg_Magenta     : String(1 .. 5) := ESC & "[35m";
      Fg_Cyan        : String(1 .. 5) := ESC & "[36m";
      Fg_White       : String(1 .. 5) := ESC & "[37m";

      -- Background colors
      Bg_Black       : String(1 .. 5) := ESC & "[40m";
      Bg_Red         : String(1 .. 5) := ESC & "[41m";
      Bg_Green       : String(1 .. 5) := ESC & "[42m";
      Bg_Yellow      : String(1 .. 5) := ESC & "[43m";
      Bg_Blue        : String(1 .. 5) := ESC & "[44m";
      Bg_Magenta     : String(1 .. 5) := ESC & "[45m";
      Bg_Cyan        : String(1 .. 5) := ESC & "[46m";
      Bg_White       : String(1 .. 5) := ESC & "[47m";

      -- Bright Foreground colors
      Br_Fg_Black    : String(1 .. 5) := ESC & "[90m";
      Br_Fg_Red      : String(1 .. 5) := ESC & "[91m";
      Br_Fg_Green    : String(1 .. 5) := ESC & "[92m";
      Br_Fg_Yellow   : String(1 .. 5) := ESC & "[93m";
      Br_Fg_Blue     : String(1 .. 5) := ESC & "[94m";
      Br_Fg_Magenta  : String(1 .. 5) := ESC & "[95m";
      Br_Fg_Cyan     : String(1 .. 5) := ESC & "[96m";
      Br_Fg_White    : String(1 .. 5) := ESC & "[97m";

      -- Bright Background colors
      Br_Bg_Black    : String(1 .. 6) := ESC & "[100m";
      Br_Bg_Red      : String(1 .. 6) := ESC & "[101m";
      Br_Bg_Green    : String(1 .. 6) := ESC & "[102m";
      Br_Bg_Yellow   : String(1 .. 6) := ESC & "[103m";
      Br_Bg_Blue     : String(1 .. 6) := ESC & "[104m";
      Br_Bg_Magenta  : String(1 .. 6) := ESC & "[105m";
      Br_Bg_Cyan     : String(1 .. 6) := ESC & "[106m";
      Br_Bg_White    : String(1 .. 6) := ESC & "[107m";
   end record;

   Colors : Colors_Type;

private

end Colors_Collection;
