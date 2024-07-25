-- File: animation.ads
-- Project: ada-output-logger
-- Created Date: 2024-07-06 16:49:08
-- Author: 3urobeat
--
-- Last Modified: 2024-07-25 19:26:23
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Strings.Bounded;
with Ada.Characters.Latin_1; -- Used for escape char carriage return
with Ada.Calendar;
with Helpers;

use Ada.Calendar;
use Helpers;


package Animation is

   -- Please keep animation frames as short as possible (<8 chars). Up to 32 Bytes are allowed to display multiple UTF-8 characters which take up >1 Byte
   package Animation_Frames_Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 32);

   type Animation_Index is range 0 .. 15;

   -- A full animation can have up to 16 frames. Fill unused frames with `others => Animation_Frames_Bounded.Null_Bounded_String`
   type Animation_Type is array(Animation_Index) of Animation_Frames_Bounded.Bounded_String;


   -- Starts an animation
   procedure Start(Animation_Frames : Animation_Type; Animation_Interval : Duration);

   -- Stops a current animation and prints the current frame, without a carriage return
   procedure Log_Static;

   -- Stops a current animation
   procedure Stop;


   -- Collection of default animations
   type Default_Animations_Type is record

      Loading : Animation_Type := (
         Animation_Frames_Bounded.To_Bounded_String(" | "),
         Animation_Frames_Bounded.To_Bounded_String(" / "),
         Animation_Frames_Bounded.To_Bounded_String(" - "),
         Animation_Frames_Bounded.To_Bounded_String(" \ "), -- You apparently do not need to escape this backslash
         others => Animation_Frames_Bounded.Null_Bounded_String
      );

      Waiting : Animation_Type := (
         Animation_Frames_Bounded.To_Bounded_String("     "),
         Animation_Frames_Bounded.To_Bounded_String(".    "),
         Animation_Frames_Bounded.To_Bounded_String("..   "),
         Animation_Frames_Bounded.To_Bounded_String("...  "),
         Animation_Frames_Bounded.To_Bounded_String(".... "),
         Animation_Frames_Bounded.To_Bounded_String("....."),
         others => Animation_Frames_Bounded.Null_Bounded_String
      );

      Bounce : Animation_Type := (
         Animation_Frames_Bounded.To_Bounded_String("=     "),
         Animation_Frames_Bounded.To_Bounded_String(" =    "),
         Animation_Frames_Bounded.To_Bounded_String("  =   "),
         Animation_Frames_Bounded.To_Bounded_String("   =  "),
         Animation_Frames_Bounded.To_Bounded_String("    = "),
         Animation_Frames_Bounded.To_Bounded_String("     ="),
         Animation_Frames_Bounded.To_Bounded_String("    = "),
         Animation_Frames_Bounded.To_Bounded_String("   =  "),
         Animation_Frames_Bounded.To_Bounded_String("  =   "),
         Animation_Frames_Bounded.To_Bounded_String(" =    "),
         others => Animation_Frames_Bounded.Null_Bounded_String
      );

      Progress : Animation_Type := (
         Animation_Frames_Bounded.To_Bounded_String("     "),
         Animation_Frames_Bounded.To_Bounded_String("█    "), -- This chonker is 3 bytes big
         Animation_Frames_Bounded.To_Bounded_String("██   "),
         Animation_Frames_Bounded.To_Bounded_String("███  "),
         Animation_Frames_Bounded.To_Bounded_String("████ "),
         Animation_Frames_Bounded.To_Bounded_String("█████"),
         others => Animation_Frames_Bounded.Null_Bounded_String
      );

      Arrows : Animation_Type := (
         Animation_Frames_Bounded.To_Bounded_String(">    "),
         Animation_Frames_Bounded.To_Bounded_String(">>   "),
         Animation_Frames_Bounded.To_Bounded_String(">>>  "),
         Animation_Frames_Bounded.To_Bounded_String(">>>> "),
         Animation_Frames_Bounded.To_Bounded_String(">>>>>"),
         others => Animation_Frames_Bounded.Null_Bounded_String
      );

      Bounce_Arrow : Animation_Type := (
         Animation_Frames_Bounded.To_Bounded_String(">    "),
         Animation_Frames_Bounded.To_Bounded_String(" >   "),
         Animation_Frames_Bounded.To_Bounded_String("  >  "),
         Animation_Frames_Bounded.To_Bounded_String("   > "),
         Animation_Frames_Bounded.To_Bounded_String("    >"),
         Animation_Frames_Bounded.To_Bounded_String("    <"),
         Animation_Frames_Bounded.To_Bounded_String("   < "),
         Animation_Frames_Bounded.To_Bounded_String("  <  "),
         Animation_Frames_Bounded.To_Bounded_String(" <   "),
         Animation_Frames_Bounded.To_Bounded_String("<    "),
         others => Animation_Frames_Bounded.Null_Bounded_String
      );

      -- Empty animation used to indicate that no animation is currently active
      None : Animation_Type := (others => Animation_Frames_Bounded.Null_Bounded_String);

   end record;

   Default_Animations : Default_Animations_Type;

private

   -- Init data storage with default values to prevent TASKING_ERROR
   Hold_Animation    : Boolean         := True; -- Init with True to prevent any unwanted iterations
   Index             : Animation_Index := Animation_Index'First;
   Interval          : Duration        := 0.5;
   Current_Animation : Animation_Type  := Default_Animations.None;

   -- Handles periodically updating an active animation
   task Animation_Updater_Task is
      entry Start;
   end Animation_Updater_Task;

end Animation;
