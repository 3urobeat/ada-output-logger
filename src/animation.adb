-- File: animation.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-06 16:49:13
-- Author: 3urobeat
--
-- Last Modified: 2024-07-08 22:29:33
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


package body Animation is

   -- Handles periodically updating an active animation
   task body Animation_Updater_Task is
      use Animation_Frames_Bounded;

      -- Store data about the animation which this task should currently handle
      Hold_Animation : Boolean;
      Index : Animation_Index;
      Interval : Duration;
      Current_Animation : Animation_Type;
   begin
      loop

         -- Use select to not block the main thread
         select
            accept Start(Animation_Frames : Animation_Type; Animation_Interval : Duration) do
               Interval          := Animation_Interval;
               Current_Animation := Animation_Frames;
               Hold_Animation    := False;

               -- Only reset animation index if a different animation was provided to provide seamless transitions between different messages with the same animation
               if Current_Animation /= Animation_Frames then
                  Index := Animation_Index'First;
               end if;
            end Start;
         or
            accept Log_Static;

            Hold_Animation := True; -- Causes the task to stop updating without resetting index
            Internal_Log("[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)) & "] ");
         or
            accept Stop;

            Current_Animation := Default_Animations.None;
            exit; -- Prevent delay from keeping task alive a little longer
         or
            delay until (Clock + Interval);

            if Current_Animation = Default_Animations.None then
               exit; -- Exit task when no animation is supposed to be running to avoid keeping the process alive
            end if;

            -- TODO: Remove animation frame from log on exit
         end select;


         if not Hold_Animation then
            -- Print this animation frame and reset cursor so the next frame can overwrite this one
            Internal_Log("[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)));
            Internal_Log("] " & Ada.Characters.Latin_1.CR);

            -- Reset index if we reached the end or the animation does not contain any more frames
            if (Index = Animation_Index'Last) or (Current_Animation(Animation_Index'Succ(Index)) = Animation_Frames_Bounded.Null_Bounded_String) then
               Index := Animation_Index'First;
            else
               Index := Animation_Index'Succ(Index); -- ...otherwise get the next element
            end if;
         end if;

      end loop;

   end Animation_Updater_Task;

end Animation;
