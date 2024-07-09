-- File: colors.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 15:27:41
-- Author: 3urobeat
--
-- Last Modified: 2024-07-09 22:38:37
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Characters.Latin_1; -- Used for escape character below

use Ada.Characters.Latin_1;


package Colors is

   Hide_Cursor : String := ESC & "[?25l";
   Show_Cursor : String := ESC & "[?25h";

   reset       : String := ESC & "[0m";
   bold        : String := ESC & "[1m";
   dim         : String := ESC & "[2m";
   italic      : String := ESC & "[3m";
   underscore  : String := ESC & "[4m";
   blink       : String := ESC & "[5m";
   background  : String := ESC & "[7m";
   hidden      : String := ESC & "[8m";
   crossed     : String := ESC & "[9m";

   fgblack     : String := ESC & "[30m";
   fgred       : String := ESC & "[31m";
   fggreen     : String := ESC & "[32m";
   fgyellow    : String := ESC & "[33m";
   fgblue      : String := ESC & "[34m";
   fgmagenta   : String := ESC & "[35m";
   fgcyan      : String := ESC & "[36m";
   fgwhite     : String := ESC & "[37m";

   bgblack     : String := ESC & "[40m";
   bgred       : String := ESC & "[41m";
   bggreen     : String := ESC & "[42m";
   bgyellow    : String := ESC & "[43m";
   bgblue      : String := ESC & "[44m";
   bgmagenta   : String := ESC & "[45m";
   bgcyan      : String := ESC & "[46m";
   bgwhite     : String := ESC & "[47m";

   brfgblack   : String := ESC & "[90m";
   brfgred     : String := ESC & "[91m";
   brfggreen   : String := ESC & "[92m";
   brfgyellow  : String := ESC & "[93m";
   brfgblue    : String := ESC & "[94m";
   brfgmagenta : String := ESC & "[95m";
   brfgcyan    : String := ESC & "[96m";
   brfgwhite   : String := ESC & "[97m";

   brbgblack   : String := ESC & "[100m";
   brbgred     : String := ESC & "[101m";
   brbggreen   : String := ESC & "[102m";
   brbgyellow  : String := ESC & "[103m";
   brbgblue    : String := ESC & "[104m";
   brbgmagenta : String := ESC & "[105m";
   brbgcyan    : String := ESC & "[106m";
   brbgwhite   : String := ESC & "[107m";

private

end Colors;
