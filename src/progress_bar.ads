-- File: progress_bar.ads
-- Project: ada-output-logger
-- Created Date: 2024-11-23 17:29:13
-- Author: 3urobeat
--
-- Last Modified: 2024-11-24 22:45:06
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Strings.Fixed;
with Terminal;
with Colors_Collection;
with Print_Manager;


package Progress_Bar is

   subtype Internal_Progress_Type is Integer range -1 .. 100;
   subtype Progress_Type          is Internal_Progress_Type range 0 .. 100;


   procedure Show_Progress_Bar(Progress : Progress_Type; New_Bar : Boolean);

end Progress_Bar;
