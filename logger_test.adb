-- File: logger_test.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 17:11:57
-- Author: 3urobeat
--
-- Last Modified: 2024-08-03 15:31:40
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


-- File for testing logger functions


-- Clean and create build folder once:
-- rm -rf ./build && mkdir ./build

-- Compile and run using: (The echo appendage makes sure your cursor reappears when the binary crashes)
-- ( $(cd build && gnatmake -g -I../src ../logger_test.adb -o logger-test) && ./build/logger-test ) ; echo -e "\033[?25h"


-- Import the library
with Logger_Type;

use Logger_Type;


-- Stuff only needed for benchmarking
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Directories;

use Ada.Real_Time;
use Ada.Text_IO;
use Ada.Characters.Latin_1;


procedure Logger_Test is
   -- Only needed for testing animation hold
   test : access Logger_Dummy;

   -- Stuff only needed for benchmarking
   Benchmark_Start    : Time;
   Benchmark_Duration : Duration;
   Color_Blue         : String := ESC & "[96m";
   Color_Reset        : String := ESC & "[0m";
   Output_File        : File_Type;
begin

   -- Get some space between us and the compile messages
   -- Logger.Nl.EoL;

   -- Test log levels with different formats
   --  Logger.Info(STR => "Hello " & Colors.Fg_Magenta & "World", SRC => "logger_test.adb").Nl.EoL; -- Test if colors can bleed into the next message. This color should also not appear in the output file
   --  Logger.Debug("Hello World").Nl.EoL;
   --  Logger.Warn("Hello", "logger_test.adb", True).Log(" World").Nl.EoL;
   --  Logger.Error("Hello World", "").Nl.EoL;


   -- Test removing message
   --  Logger.Rm.Info("Long message that should be gone in 500ms").EoL;
   --  delay 0.5;
   --  Logger.Info("Hello Again").Nl.EoL;

   --  Logger.Rm.Info("Short msg").EoL;
   --  delay 0.5;
   --  Logger.Info("Hello Again using a longer message").Nl.EoL;

   --  Logger.Rm.Info("Long message that should get overwritten by Finalize when the process exits or Logger gets deleted").EoL;
   --  delay 0.5;


   -- Test removing part of message printed after newline
   --  Logger.Info("First part of the messageFirst part of the messageFirst part of the messageFirst part of the message").Nl.Rm.Log("Second part which should be removed").EoL; -- This is supposed to check if resetting Message_Length on Nl is correct


   -- Test animations
   --  Logger.Rm.Animate(Default_Animations.Loading).Info("Hello there").EoL;
   --  delay 2.4;
   --  Logger.Rm.Animate(Default_Animations.Waiting).Warn("Next").EoL;  -- Start another animation to stop the previous one
   --  Logger.Rm.Animate(Default_Animations.Loading).Warn("Next").EoL;  -- ...or continue with the same animation using a different message to showcase the frame retention

   --  test := Logger.Animate(Default_Animations.Loading);  -- Simulate that it takes time to change the message. The animation should retain the last frame and hold
   --  delay 0.9;
   --  test.Rm.Warn("Next").EoL;

   --  delay 2.0;
   --  Logger.Animate(Default_Animations.Waiting).Error("Test message").EoL;  -- Test "boop" appearing behind reprinted animation in output.txt
   --  delay 2.1;
   --  Logger.Log("boop").EoL;
   --  Logger.Rm.Animate(Default_Animations.Arrows).Debug("This should be gone").EoL;
   --  delay 0.4;


   -- Test newline printing after animation is stopped
   --  Logger.Animate(Default_Animations.Arrows).Info("This is not illegal anymore").Nl.EoL;
   --  delay 0.7;
   --  Logger.Info("Boop").Nl.EoL;
   --  Logger.Rm.Warn("This is illegal").Nl.EoL;
   --  Logger.Rm.Animate(Default_Animations.Waiting).Warn("This is illegal as well").Nl.EoL;


   -- Test cutting long messages to terminal width
   --  Logger.Rm.Debug("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").Log("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb").EoL; -- Simple test

   --  test := Logger.Rm.Debug("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
   --  delay 5.0;                                                                                                        -- Resize terminal during print
   --  test.Log("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb").EoL;
   --  delay 2.0;


   -- Test reading input
   declare
      User_Input_No_Timeout   : access String := Logger.Read_Input("Please submit your name: ");
      User_Input_With_Timeout : access String := Logger.Read_Input("Please submit your name: ", 2.5);
   begin
      Logger.Info("User is called " & User_Input_No_Timeout).Nl.EoL;

      -- Differentiate between input-less submit and timeout
      if User_Input_With_Timeout = null then
         Logger.Warn("Input timeout detected!").Nl.EoL;
      elsif User_Input_With_Timeout.all = "" then
         Logger.Warn("User did not provide an input!").Nl.EoL;
      else
         Logger.Info("User is called " & User_Input_With_Timeout.all).Nl.EoL;
      end if;
   end;


   -- Bechmark
   --  Benchmark_Duration := 0.0;    -- Reset;

   --  for i in 0 .. 19 loop         -- Use Put_Line 20 times and calculate average
   --     Benchmark_Start := Clock;
   --     Logger.Info("Hello World", "logger_test.adb").Nl.EoL;
   --     Benchmark_Duration := Benchmark_Duration + (To_Duration(Clock - Benchmark_Start) * 1000000.0); -- Seconds -> Microseconds
   --  end loop;

   --  Logger.Log("Logger took an average of " & Duration'Image(Benchmark_Duration / 20) & "μs over 20 attempts").Nl.EoL;

   --  Benchmark_Duration := 0.0;    -- Reset;

   --  if Ada.Directories.Exists("./output2.txt") = False then
   --     Create(Output_File, Append_File, "./output2.txt");
   --  else
   --     Open(Output_File, Append_File, "./output2.txt");
   --  end if;

   --  for i in 0 .. 19 loop         -- Use Put_Line 20 times and calculate average
   --     Benchmark_Start := Clock;
   --     Put_Line("[" & Color_Blue & Ada.Calendar.Formatting.Image(Ada.Calendar.Clock) & Color_Reset & "] [" & Color_Blue & "INFO" & Color_Reset & " | " & Color_Blue & "logger_test.adb" & Color_Reset & "] Hello World");
   --     Put_Line(Output_File, "[" & Ada.Calendar.Formatting.Image(Ada.Calendar.Clock) & "] [INFO | logger_test.adb] Hello World");
   --     Benchmark_Duration := Benchmark_Duration + (To_Duration(Clock - Benchmark_Start) * 1000000.0); -- Seconds -> Microseconds
   --  end loop;

   --  Logger.Log("Put_Line took an average of " & Duration'Image(Benchmark_Duration / 20) & "μs over 20 attempts").Nl.EoL;



   -- Exit when animation is active
   --  Logger.Stop_Animation; -- Exit
   --  Logger.Info("Please exit").Nl.EoL; -- ...or exit by printing a message without animation

   null;

end Logger_Test;
