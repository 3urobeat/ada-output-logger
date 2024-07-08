-- File: logger_type.ads
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-08 17:13:45
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Finalization;
with Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Characters.Latin_1; -- Used for escape character carriage return & newline
with Animation;
with Colors;
with Construct;
with File_Output;
with Helpers;

use Ada.Text_IO;
use Animation;
use Construct;
use Helpers;


package Logger_Type is

   -- Expose set of default animations for easy access
   Default_Animations : Default_Animations_Type := Animation.Default_Animations;


   -- Get a 128 Byte bounded string for options below
   package Options_Bounded_128B is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 128);

   -- The default Logger instance, containing default settings
   type Logger_Dummy is new Ada.Finalization.Controlled with record

      -- Time in ms to wait between refreshing message with the next animation frame
      Animate_Interval : Duration := 0.5;

      -- Set a string that shall be printed when the program exits/the Logger instance is deleted. Set to empty to exit silently.
      Exit_Message : Options_Bounded_128B.Bounded_String := Options_Bounded_128B.To_Bounded_String("Goodbye!");

      -- Relative path from binary location to a file where log messages should be written to. Set to empty to disable this feature.
      Output_File_Path : Options_Bounded_128B.Bounded_String := Options_Bounded_128B.To_Bounded_String("./output.txt");


      -- Internal: Tracks length of the current message before EoL was called
      Current_Message_Length : Natural := 0;

      -- Internal: Tracks length of the previous message (if it was marked as Rm) to overwrite ghost chars
      Last_Message_Length : Natural := 0;

      -- Internal: Tracks the currently active animation
      Current_Animation : Animation_Type := Default_Animations.None;

   end record;

   -- Internal: Overwrite Finalize to catch when Logger is deleted
   procedure Finalize(this : in out Logger_Dummy); -- TODO: I wish I could private this


   -- Prepends the following message with an animation. The animation will be refreshed every Animate_Interval ms as long as it is not canceled by logging another message. Call this before any other logger function.
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param ANIM The animation to start displaying
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Animate(this : access Logger_Dummy; ANIM : Animation_Type) return access Logger_Dummy;

   -- Stops an active animation
   -- @param this Instance of Logger, automatically provided when using dot notation
   procedure Stop_Animation(this : access Logger_Dummy);

   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Log(this : access Logger_Dummy; STR : String) return access Logger_Dummy;

   -- Logs a message to stdout with 'INFO' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Info(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy;

   -- Logs a message to stdout with 'DEBUG' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Debug(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy;

   -- Logs a message to stdout with 'WARN' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Warn(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy;

   -- Logs a message to stdout with 'ERROR' prefix
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @param STR The message to log
   -- @param SRC Optional: Name of the file this log message originates from
   -- @param ND Optional: No-Date - Set to true if your message should not include a timestamp
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Error(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy;

   -- Logs a newline to stdout
   -- @param this Instance of Logger, automatically provided when using dot notation
   -- @return Returns `this` instance of Logger to support chaining another function call
   function Nl(this : access Logger_Dummy) return access Logger_Dummy;

   -- Marks this message to be overwritten by the next logger call and ends the message
   -- @param this Instance of Logger, automatically provided when using dot notation
   procedure RmEoL(this : access Logger_Dummy);

   -- Ends the message. This is a required dummy function as Ada forces us to process return values, which we don't want when being done calling Logger functions
   -- @param this Instance of Logger, automatically provided when using dot notation
   procedure EoL(this : access Logger_Dummy);


   -- Create instance of Logger_Dummy for everyone to use
   Logger : aliased Logger_Dummy; -- TODO: Does it have disadvantages marking this as aliased?

private

end Logger_Type;
