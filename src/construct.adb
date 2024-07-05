-- File: construct.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 18:23:08
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


package body Construct is

    -- Returns the current timestamp formatted as ISO 8601
   function Get_Timestamp return String is
   begin
      return Image(Clock);
   end Get_Timestamp;


   -- Constructs prefix of the message to be logged and returns it
   function Construct_Message_Prefix(Log_Lvl : String; Origin : String; No_Date : Boolean) return String is
      Temp : Construct_Temp_String.Bounded_String;
   begin

      -- Start with first bracket
      Construct_Temp_String.Append(Temp, "[");


      -- Append date if enabled
      if No_Date = False then
         Construct_Temp_String.Append(Temp, Colors.brfgcyan);
         Construct_Temp_String.Append(Temp, Get_Timestamp);
         Construct_Temp_String.Append(Temp, Colors.reset);
      end if;


      -- Close date bracket if Origin was specified and open another one, otherwise append log level directly
      if Origin'Length > 0 then
         if No_Date = False then
            Construct_Temp_String.Append(Temp, "] [");
         end if;

         Construct_Temp_String.Append(Temp, Log_Lvl);
         Construct_Temp_String.Append(Temp, " | ");
         Construct_Temp_String.Append(Temp, Origin);
         Construct_Temp_String.Append(Temp, "]");

      else

         if No_Date = False then
            Construct_Temp_String.Append(Temp, " | ");
         end if;

         Construct_Temp_String.Append(Temp, Log_Lvl);
         Construct_Temp_String.Append(Temp, "]");
      end if;


      -- Color message red if log lvl is error


      -- Return constructed string
      return Construct_Temp_String.To_String(Temp) & " ";

   end Construct_Message_Prefix;


   -- Construct suffix of the message to be logged and returns it
   function Construct_Message_Suffix(Remove : Boolean) return String is
   begin

      -- Return carriage return if remove is enabled
      if Remove = True then
         return "" & Ada.Characters.Latin_1.CR;
      else
         return "";
      end if;

   end Construct_Message_Suffix;

end Construct;
