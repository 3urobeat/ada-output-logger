-- File: animation.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-06 16:49:13
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


package body Animation is

   -- Starts an animation
   procedure Start(Animation_Frames : Animation_Type; Animation_Interval : Duration) is
   begin
      -- Only reset animation index if a different animation was provided to provide seamless transitions between different messages with the same animation
      if Current_Animation /= Animation_Frames then
         Index := Animation_Index'First;
      end if;

      Current_Animation := Animation_Frames;
      Interval          := Animation_Interval;
      Hold_Animation    := False;

      Animation_Updater_Task.Start;
   end Start;


   -- Stops a current animation and prints the current frame, without a carriage return
   procedure Log_Static is
   begin
      Hold_Animation := True; -- Causes the task to stop updating without resetting index
      Internal_Log("[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)) & "] ");
   end Log_Static;


   -- Stops a current animation
   procedure Stop is
   begin
      Hold_Animation := True;
      Current_Animation := Default_Animations.None;
   end Stop;


   -- Handles periodically updating an active animation
   task body Animation_Updater_Task is
      use Animation_Frames_Bounded;

      Next_Run : Time := Clock;
   begin
      -- This select prevents the task from keeping the process alive when no animations have been started during the application's lifetime
      select
         accept Start;
      or
         terminate;
      end select;

      while not Hold_Animation loop
         if Clock >= Next_Run then

            -- Print this animation frame and reset cursor so the next frame can overwrite this one
            Internal_Log("[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)));
            Internal_Log("] " & Ada.Characters.Latin_1.CR);

            -- Reset index if we reached the end or the animation does not contain any more frames
            if (Index = Animation_Index'Last) or (Current_Animation(Animation_Index'Succ(Index)) = Animation_Frames_Bounded.Null_Bounded_String) then
               Index := Animation_Index'First;
            else
               Index := Animation_Index'Succ(Index); -- ...otherwise get the next element
            end if;

            Next_Run := Clock + Interval;

         end if;

         delay 0.01; -- Delay next check a little to reduce CPU usage
      end loop;

      -- TODO: Remove animation frame from log on exit
   end Animation_Updater_Task;

end Animation;
