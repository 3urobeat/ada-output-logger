-- File: progress_bar.adb
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


package body Progress_Bar is

   procedure Show_Progress_Bar(Progress : Progress_Type; New_Bar : Boolean) is
      Bar_Width       : Positive := Terminal.Get_Terminal_Width - 1; -- Subtract 1 for terminal width tolerance
      Str             : String(1 .. Bar_Width + 14);                 -- +14 to compensate for color codes
      Hashtags_Amount : Positive;

   begin
      -- TODO: Check if String is too short. Display only percentage or nothing at all.

      -- Init progress string with label
      Str(Str'First .. 22) := Colors_Collection.Colors.Br_Bg_Green & Colors_Collection.Colors.Fg_Black & "Progress: [";

      -- Add current progress. We are using Tail to pad Progress (after trimming it to remove the uncontrolled included padding) so that it's always 3 wide
      Str(23 .. 33) :=
         Ada.Strings.Fixed.Tail(
            Ada.Strings.Fixed.Trim(Integer'Image(Progress), Ada.Strings.Left), -- This call is quite verbose and kinda sucks
            3,
            ' ')
         & "%]"
         & Colors_Collection.Colors.Reset
         & " [";

      -- Calculate the amount of hashtags to display and append them. Subtract 19 for the label and brackets
      Hashtags_Amount := Positive(Float'Rounding(Float(Bar_Width - 19) * (Float(Progress) / 100.0))); -- Sorry for the amount of conversions

      Str(20 + 14 .. Hashtags_Amount)                := (others => '#');
      Str(20 + 14 + Hashtags_Amount .. Str'Last - 1) := (others => ' ');

      -- Append ending bracket
      Str(Str'Last .. Str'Last) := "]"; -- I'm kinda confused how Last till Last works here but it works like that so ??

      -- Submit bar to Print_Manager
      if New_Bar then
         Print_Manager.Print(Print_Manager.Progress_Create, Str);
      else
         Print_Manager.Print(Print_Manager.Progress_Update, Str);
      end if;
   end Show_Progress_Bar;

end Progress_Bar;
