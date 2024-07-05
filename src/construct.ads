-- File: construct.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 18:23:04
-- Author: 3urobeat
--
-- Last Modified: 2024-07-05 15:19:13
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Colors;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Bounded;
with Ada.Characters.Latin_1; -- Used for escape character for carriage return

use Ada.Calendar;
use Ada.Calendar.Formatting;


package Construct is

   -- Buffer for constructing message prefix
   package Construct_Temp_String is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 128);


   -- Returns the current timestamp formatted as ISO 8601
   function Get_Timestamp return String;

   -- Constructs prefix of the message to be logged and returns it
   function Construct_Message_Prefix(Log_Lvl : String; Origin : String; No_Date : Boolean) return String;

   -- Construct suffix of the message to be logged and returns it
   function Construct_Message_Suffix(Remove : Boolean) return String;

private

end Construct;
