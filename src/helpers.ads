-- File: helpers.ads
-- Project: ada-output-logger
-- Created Date: 2024-07-03 18:53:35
-- Author: 3urobeat
--
-- Last Modified: 2024-07-06 15:47:48
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

    -- Internal: Expands Construct_Message_Prefix call to construct a log message prefix
    -- @param Color Optional: Color code for log level parameter
    -- @param Lvl Log level of this message
    -- @param SRC Optional: User provided name of the file this log message originates from
    -- @param ND Optional: No-Date - User provided setting if date should not include a timestamp
    -- @return Returns the formatted message prefix string
    function Get_Prefix(Color : String := ""; Lvl : String; SRC : String := ""; ND : Boolean := False) return String;

    -- Internal: Constructs a String of whitespaces to concat to a log message if the last message was longer than the current message is. This prevents ghost chars from messages marked as Rm.
    -- @param Current_Message_Length Length of the current message
    -- @param Last_Message_Length Length of the last message
    -- @return Returns string to append to the current message
    function Get_Trailing_Whitespaces(Current_Message_Length : Natural; Last_Message_Length : Natural) return String;

private

end Helpers;
