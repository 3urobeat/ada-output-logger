-- File: construct.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 18:23:04
-- Author: 3urobeat
--
-- Last Modified: 2024-07-27 17:00:01
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Bounded;
with Ada.Characters.Latin_1; -- Used for escape character for carriage return
with Colors_Collection;

use Ada.Calendar;
use Ada.Calendar.Formatting;
use Colors_Collection;


package Construct is

   -- Buffer for constructing message prefix
   package Construct_Bounded_128B is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 128);


   -- Returns the current timestamp formatted as ISO 8601
   function Get_Timestamp return String;

   -- Constructs prefix of the message to be logged and returns it
   function Construct_Message_Prefix(Log_Lvl : String; Origin : String; No_Date : Boolean := False; No_Color : Boolean := False) return String;

private

end Construct;
