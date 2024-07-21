-- File: logger_test.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 17:11:57
-- Author: 3urobeat
--
-- Last Modified: 2024-07-21 22:25:49
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

-- Compile and run using:
-- $(cd build && gnatmake -I../src ../logger_test.adb -o logger-test) && ./build/logger-test


with Logger_Type;

use Logger_Type;


procedure Logger_Test is
   test : access Logger_Dummy;
begin

   -- Get some space between us and the compile messages
   Logger.Nl.EoL;

   -- Test log levels with different formats
   Logger.Info(STR => "Hello World", SRC => "logger_test.adb").Nl.EoL;
   Logger.Debug("Hello World").Nl.EoL;
   Logger.Warn("Hello", "logger_test.adb", True).Log(" World").Nl.EoL;
   Logger.Error("Hello World", "").Nl.EoL;


   -- Test removing message
   --  Logger.Info("Long message that should be gone in 500ms").RmEoL;
   --  delay 0.5;
   --  Logger.Info("Hello Again").Nl.EoL;

   --  Logger.Info("Short msg").RmEoL;
   --  delay 0.5;
   --  Logger.Info("Hello Again using a longer message").Nl.EoL;

   --  Logger.Info("Long message that should get overwritten by Finalize when the process exits or Logger gets deleted").RmEoL;
   --  delay 0.5;


   -- Test animations
   Logger.Animate(Default_Animations.Loading).Info("Hello there").RmEoL;
   delay 2.25;
   --Logger.Animate(Default_Animations.Waiting).Warn("Next").RmEoL; -- Start another animation to stop the previous one
   --Logger.Animate(Default_Animations.Loading).Warn("Next").RmEoL; -- ...or continue with the same animation using a different message to showcase the frame retention

   --test := Logger.Animate(Default_Animations.Loading); -- Simulate that it takes time to change the message. The animation should retain the last frame and hold
   --delay 0.9;
   --test.Warn("Next").RmEoL;

   --delay 2.0;
   Logger.Animate(Default_Animations.Waiting).Error("Test message").EoL; -- Test EoL() detecting active animation and calling RmEoL() instead

   --Logger.Stop_Animation; -- Exit
   Logger.Info("Please exit").Nl.EoL; -- ...or exit by printing a message without animation

   --Logger.Animate(Default_Animations.Arrows).Warn("This is illegal").Nl.RmEoL; -- This should throw an Illegal_Newline exception

end Logger_Test;
