-- File: animation.adb
-- Project: ada-output-logger
-- Created Date: 2024-07-06 16:49:13
-- Author: 3urobeat
--
-- Last Modified: 2024-07-30 16:40:43
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


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
   function Log_Static return String is
   begin
      Hold_Animation := True; -- Causes the task to stop updating without resetting index
      return "[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)) & "] ";
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

      Next_Run : Time;
   begin
      loop        -- This loop ensures the task can be started multiple times during the application's lifetime
         select   -- This select prevents the task from keeping the process alive when no animations have been started during the application's lifetime
            accept Start;
            Next_Run := Clock;
         or
            terminate;
         end select;

         while not Hold_Animation loop
            if Clock >= Next_Run then

               -- Print this animation frame and reset cursor so the next frame can overwrite this one
               Ada.Text_IO.Put("[" & Animation_Frames_Bounded.To_String(Current_Animation(Index)));      -- Note: Should you want to use Internal_Log again, these messages must not be counted in message length and not appended to the reprint buffer!
               Ada.Text_IO.Put("] " & Ada.Characters.Latin_1.CR);

               -- Reset index if we reached the end or the animation does not contain any more frames
               if (Index = Animation_Index'Last) or (Current_Animation(Animation_Index'Succ(Index)) = Animation_Frames_Bounded.Null_Bounded_String) then
                  Index := Animation_Index'First;
               else
                  Index := Animation_Index'Succ(Index); -- ...otherwise get the next element
               end if;

               Next_Run := Clock + Interval;

            end if;

            -- Delay next check a little to reduce CPU usage
            --delay 0.01; -- TODO: This somehow causes the main thread to hold on the second animation?
         end loop;
      end loop;

      -- TODO: Remove animation frame from log on exit
   end Animation_Updater_Task;

end Animation;
