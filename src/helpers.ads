-- File: helpers.ads
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-07-03 18:53:35
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Colors;
with Construct;

use Construct;


package Helpers is

    -- Internal: Expands Construct_Message_Prefix call
    function Get_Prefix(Color : String; Lvl : String; SRC : String; ND : Boolean) return String;

private

end Helpers;
