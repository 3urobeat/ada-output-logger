-- File: logger_test.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 17:11:57
-- Author: 3urobeat
--
-- Last Modified: 2024-07-05 15:27:22
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
begin

   -- Get some space between us and the compile messages
   Logger.Nl.EoL;

   Logger.Info(STR => "Hello World", SRC => "logger_test.adb").Nl.EoL;
   Logger.Debug("Hello World").Nl.EoL;
   Logger.Warn("Hello", "logger_test.adb", True).Log(" World").Nl.EoL;
   Logger.Error("Hello World", "").RmEoL;

   Logger.Info("Hello Again").Nl.EoL;

end Logger_Test;
