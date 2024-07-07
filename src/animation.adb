-- File: animation.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-06 16:49:13
-- Author: 3urobeat
--
-- Last Modified: 2024-07-07 19:30:54
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

      -- Stores location of where the active animation is registered
      Animation_Storage : access Animation_Type;

      -- Stores data about the animation this task currently handles
      Index : Animation_Index;
      Interval : Duration;
      Current_Animation : Animation_Type;
   begin
      loop
         -- Use select to make calling Start not block the main thread
         select

            -- Starts to handle an animation if none is currently running
            accept Start(Animation_Frames : aliased in out Animation_Type; Animation_Interval : Duration) do

               -- Ignore call if no animation is registered
               if Animation_Frames = Default_Animations.None then
                  return;
               end if;

               -- Let me explain: We need a pointer to Logger_Dummy.Current_Animation to be able to abort the while loop below. This local Animation_Storage type should not have a longer lifetime than Logger, since this however is a possible outcome, Ada will only allow an unchecked access
               Animation_Storage := Animation_Frames'Unchecked_Access; -- TODO: Refactor to use an object or something that can be destroyed instead of using an Unchecked_Access

               -- Populate task storage
               Index             := Animation_Index'First;
               Interval          := Animation_Interval;
               Current_Animation := Animation_Frames;

            end Start;

         or
            -- Allow task to be terminated while waiting
            terminate;
         end select;


         -- Process animation when Start has been called
         while (Animation_Storage /= null) and (Animation_Storage.all = Current_Animation) loop

            -- Print this animation frame and reset cursor so the next frame can overwrite this one
            Internal_Log("[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)) & Ada.Characters.Latin_1.CR);

            -- Reset index if we reached the end or the animation does not contain any more frames
            if (Index = Animation_Index'Last) or (Current_Animation(Animation_Index'Succ(Index)) = Animation_Frames_Bounded.Null_Bounded_String) then
               Index := Animation_Index'First;
            else
               Index := Animation_Index'Succ(Index); -- ...otherwise get the next element
            end if;

            delay Interval; -- TODO: Improve by using delay until

         end loop;

         -- Reset animation copy stored in task when animation exits
         Animation_Storage := null;
         Index             := Animation_Index'First;
         Current_Animation := Default_Animations.None;

         -- TODO: Remove animation frame from log on exit
      end loop;

   end Animation_Updater_Task;

end Animation;
