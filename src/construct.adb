-- File: construct.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 18:23:08
-- Author: 3urobeat
--
-- Last Modified: 2024-07-06 15:29:50
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


package body Construct is

   -- Returns the current timestamp formatted as ISO 8601
   function Get_Timestamp return String is
   begin
      return Image(Clock); -- TODO: Returns UTC, local might be better I think
   end Get_Timestamp;


   -- Constructs prefix of the message to be logged and returns it
   function Construct_Message_Prefix(Log_Lvl : String; Origin : String; No_Date : Boolean := False; No_Color : Boolean := False) return String is
      Temp : Construct_Bounded_128B.Bounded_String;
   begin

      -- Start with first bracket
      Construct_Bounded_128B.Set_Bounded_String(Temp, "[");


      -- Append date if enabled
      if No_Date = False then
         if No_Color = False then
            Construct_Bounded_128B.Append(Temp, Colors.brfgcyan & Get_Timestamp & Colors.reset);
         else
            Construct_Bounded_128B.Append(Temp, Get_Timestamp);
         end if;
      end if;


      -- Close date bracket if Origin was specified and open another one, otherwise append log level directly
      if Origin'Length > 0 then
         if No_Date = False then
            Construct_Bounded_128B.Append(Temp, "] [");
         end if;

         Construct_Bounded_128B.Append(Temp, Log_Lvl);
         Construct_Bounded_128B.Append(Temp, " | ");
         Construct_Bounded_128B.Append(Temp, Origin);
         Construct_Bounded_128B.Append(Temp, "]");

      else

         if No_Date = False then
            Construct_Bounded_128B.Append(Temp, " | ");
         end if;

         Construct_Bounded_128B.Append(Temp, Log_Lvl);
         Construct_Bounded_128B.Append(Temp, "]");
      end if;


      -- Return constructed string
      return Construct_Bounded_128B.To_String(Temp) & " ";

   end Construct_Message_Prefix;

end Construct;
