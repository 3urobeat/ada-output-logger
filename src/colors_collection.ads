-- File: colors_collection.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 15:27:41
-- Author: 3urobeat
--
-- Last Modified: 2024-07-27 17:00:01
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Characters.Latin_1; -- Used for escape character below

use Ada.Characters.Latin_1;


package Colors_Collection is

   type Colors_Type is record
      Hide_Cursor : String(1 .. 6) := ESC & "[?25l";
      Show_Cursor : String(1 .. 6) := ESC & "[?25h";

      reset       : String(1 .. 4) := ESC & "[0m";
      bold        : String(1 .. 4) := ESC & "[1m";
      dim         : String(1 .. 4) := ESC & "[2m";
      italic      : String(1 .. 4) := ESC & "[3m";
      underscore  : String(1 .. 4) := ESC & "[4m";
      blink       : String(1 .. 4) := ESC & "[5m";
      background  : String(1 .. 4) := ESC & "[7m";
      hidden      : String(1 .. 4) := ESC & "[8m";
      crossed     : String(1 .. 4) := ESC & "[9m";

      fgblack     : String(1 .. 5) := ESC & "[30m";
      fgred       : String(1 .. 5) := ESC & "[31m";
      fggreen     : String(1 .. 5) := ESC & "[32m";
      fgyellow    : String(1 .. 5) := ESC & "[33m";
      fgblue      : String(1 .. 5) := ESC & "[34m";
      fgmagenta   : String(1 .. 5) := ESC & "[35m";
      fgcyan      : String(1 .. 5) := ESC & "[36m";
      fgwhite     : String(1 .. 5) := ESC & "[37m";

      bgblack     : String(1 .. 5) := ESC & "[40m";
      bgred       : String(1 .. 5) := ESC & "[41m";
      bggreen     : String(1 .. 5) := ESC & "[42m";
      bgyellow    : String(1 .. 5) := ESC & "[43m";
      bgblue      : String(1 .. 5) := ESC & "[44m";
      bgmagenta   : String(1 .. 5) := ESC & "[45m";
      bgcyan      : String(1 .. 5) := ESC & "[46m";
      bgwhite     : String(1 .. 5) := ESC & "[47m";

      brfgblack   : String(1 .. 5) := ESC & "[90m";
      brfgred     : String(1 .. 5) := ESC & "[91m";
      brfggreen   : String(1 .. 5) := ESC & "[92m";
      brfgyellow  : String(1 .. 5) := ESC & "[93m";
      brfgblue    : String(1 .. 5) := ESC & "[94m";
      brfgmagenta : String(1 .. 5) := ESC & "[95m";
      brfgcyan    : String(1 .. 5) := ESC & "[96m";
      brfgwhite   : String(1 .. 5) := ESC & "[97m";

      brbgblack   : String(1 .. 6) := ESC & "[100m";
      brbgred     : String(1 .. 6) := ESC & "[101m";
      brbggreen   : String(1 .. 6) := ESC & "[102m";
      brbgyellow  : String(1 .. 6) := ESC & "[103m";
      brbgblue    : String(1 .. 6) := ESC & "[104m";
      brbgmagenta : String(1 .. 6) := ESC & "[105m";
      brbgcyan    : String(1 .. 6) := ESC & "[106m";
      brbgwhite   : String(1 .. 6) := ESC & "[107m";
   end record;

   Colors : Colors_Type;

private

end Colors_Collection;
