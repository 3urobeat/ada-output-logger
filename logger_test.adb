-- File: logger_test.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 17:11:57
-- Author: 3urobeat
--
-- Last Modified: 2024-07-27 17:00:01
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


-- File for testing logger functions


-- Clean and create build folder once:
-- rm -rf ./build && mkdir ./build

-- Compile and run using: (The echo appendage makes sure your cursor reappears when the binary crashes)
-- ( $(cd build && gnatmake -g -I../src ../logger_test.adb -o logger-test) && ./build/logger-test ) ; echo -e "\033[?25h"


with Ada.Text_IO;
with Logger_Type;
with Terminal;
with Helpers;

use Logger_Type;


procedure Logger_Test is
   test : access Logger_Dummy;
begin

   -- Get some space between us and the compile messages
   Logger.Nl.EoL;

   -- Test log levels with different formats
   Logger.Info(STR => "Hello " & Colors.fgmagenta & "World", SRC => "logger_test.adb").Nl.EoL; -- Test if colors can bleed into the next message
   Logger.Debug("Hello World").Nl.EoL;
   Logger.Warn("Hello", "logger_test.adb", True).Log(" World").Nl.EoL;
   Logger.Error("Hello World", "").Nl.EoL;


   -- Test removing message
   --  Logger.Rm.Info("Long message that should be gone in 500ms").EoL;
   --  delay 0.5;
   --  Logger.Info("Hello Again").Nl.EoL;

   --  Logger.Rm.Info("Short msg").EoL;
   --  delay 0.5;
   --  Logger.Info("Hello Again using a longer message").Nl.EoL;

   --  Logger.Rm.Info("Long message that should get overwritten by Finalize when the process exits or Logger gets deleted").EoL;
   --  delay 0.5;


   -- Test animations
   --  Logger.Animate(Default_Animations.Loading).Info("Hello there").EoL;
   --  delay 2.4;
   --  Logger.Animate(Default_Animations.Waiting).Warn("Next").EoL; -- Start another animation to stop the previous one
   --  Logger.Animate(Default_Animations.Loading).Warn("Next").EoL; -- ...or continue with the same animation using a different message to showcase the frame retention

   --  test := Logger.Animate(Default_Animations.Loading); -- Simulate that it takes time to change the message. The animation should retain the last frame and hold
   --  delay 0.9;
   --  test.Rm.Warn("Next").EoL;

   --  delay 2.0;
   Logger.Animate(Default_Animations.Waiting).Error("Test message").EoL; -- Test keeping message without animation frame in stdout


   -- Test illegal newline detection
   --  Logger.Animate(Default_Animations.Arrows).Warn("This is illegal").Nl.EoL;
   --  Logger.Rm.Warn("This is also illegal").Nl.EoL;


   -- Test cutting long messages to terminal width
   --  Ada.Text_IO.Put_Line(Terminal.Get_Terminal_Width'Image);                           -- Check width readout of this terminal
   --  Ada.Text_IO.Put_Line(Helpers.Cut_To_Terminal_Width("abcdefghijklmnopqrstuvwxyz"));
   Logger.Rm.Debug("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").EoL;
   delay 2.0;


   -- Exit when animation is active
   --Logger.Stop_Animation; -- Exit
   --Logger.Info("Please exit").Nl.EoL; -- ...or exit by printing a message without animation

end Logger_Test;
