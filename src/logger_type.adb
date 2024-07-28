-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-07-28 15:11:52
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


with Ada.Exceptions;

use Ada.Exceptions;


package body Logger_Type is

   -- Internal: Overwrite Initialize to catch when Logger is instantiated
   procedure Initialize(this : in out Logger_Dummy) is
   begin
      Put(Colors.Hide_Cursor);
   end Initialize;


   -- Internal: Overwrite Finalize to catch when Logger is deleted
   procedure Finalize(this : in out Logger_Dummy) is
      Exit_Msg : String := Options_Bounded_128B.To_String(this.Exit_Message);
   begin
      Put(Colors.Show_Cursor);
      this.Log(Exit_Msg).EoL;
   exception                              -- TODO: Should be removed in release build for less overhead
      when E : others =>
         Put_Line("Logger Exception: " & Exception_Information(E));
   end Finalize;


   -- Create Logger instance for everyone to use after Initialize has been processed in the elaboration phase
   Logger_Instance : aliased Logger_Dummy;

   function Logger return access Logger_Dummy is
      this : access Logger_Dummy := Logger_Instance'Access;
   begin
      -- Reset rm & animation submit
      this.Marked_As_Rm     := False;
      this.Submit_Animation := False;

      return this;
   end Logger;



   -- Marks this message to be overwritten by the next logger call
   function Rm(this : access Logger_Dummy) return access Logger_Dummy is
   begin
      this.Marked_As_Rm := True;

      return this;
   end Rm;


   -- Prepends the following message with an animation. The animation will be refreshed every  Call this before any
   function Animate(this : access Logger_Dummy; ANIM : Animation_Type) return access Logger_Dummy is
   begin
      this.Submit_Animation := True;

      -- Check if there is a running animation. If it is the same, get it printed and hold
      if this.Current_Animation = ANIM then
         Animation.Log_Static(this.Current_Message_Length); -- This prints the current animation frame once to offset the following message content
      else
         Animation.Stop;
         Internal_Log("[" & Animation_Frames_Bounded.To_String(ANIM(Animation_Index'First)) & "] ", this.Current_Message_Length);
      end if;

      -- Register this animation
      this.Current_Animation := ANIM;

      -- Note: The animation handler task will be started by RmEoL
      return this;
   end Animate;


   -- Stops an active animation
   procedure Stop_Animation(this : access Logger_Dummy) is
   begin
      this.Current_Animation := Default_Animations.None;
      Animation.Stop;
   end Stop_Animation;


   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   function Log(this : access Logger_Dummy; STR : String) return access Logger_Dummy is
   begin
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), STR);
      Internal_Log(STR, this.Current_Message_Length, this.Marked_As_Rm or this.Submit_Animation);

      return this;
   end Log;


   -- Logs a message to stdout with 'INFO' prefix
   function Info(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      Internal_Prefixed_Log(
         Output_File_Path        => Options_Bounded_128B.To_String(this.Output_File_Path),
         Current_Message_Length  => this.Current_Message_Length,
         Log_Lvl                 => "INFO",
         Color                   => Colors.brfgcyan,
         STR                     => STR,
         SRC                     => SRC,
         ND                      => ND,
         Cut                     => this.Marked_As_Rm or this.Submit_Animation
      );

      return this;
   end Info;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      Internal_Prefixed_Log(
         Output_File_Path        => Options_Bounded_128B.To_String(this.Output_File_Path),
         Current_Message_Length  => this.Current_Message_Length,
         Log_Lvl                 => "DEBUG",
         Color                   => Colors.brfgcyan & Colors.background,
         STR                     => STR,
         SRC                     => SRC,
         ND                      => ND,
         Cut                     => this.Marked_As_Rm or this.Submit_Animation
      );

      return this;
   end Debug;


   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      Internal_Prefixed_Log(
         Output_File_Path        => Options_Bounded_128B.To_String(this.Output_File_Path),
         Current_Message_Length  => this.Current_Message_Length,
         Log_Lvl                 => "WARN",
         Color                   => Colors.fgred,
         STR                     => STR,
         SRC                     => SRC,
         ND                      => ND,
         Cut                     => this.Marked_As_Rm or this.Submit_Animation
      );

      return this;
   end Warn;


   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      Internal_Prefixed_Log(
         Output_File_Path        => Options_Bounded_128B.To_String(this.Output_File_Path),
         Current_Message_Length  => this.Current_Message_Length,
         Log_Lvl                 => "ERROR",
         Color                   => Colors.fgred & Colors.background,
         STR                     => Colors.fgred & STR,
         SRC                     => SRC,
         ND                      => ND,
         Cut                     => this.Marked_As_Rm or this.Submit_Animation
      );

      return this;
   end Error;


   -- Logs a newline to stdout
   function Nl(this : access Logger_Dummy) return access Logger_Dummy is
   begin
      -- Deny newline call for messages awaiting carriage return as this would break overwriting it
      if this.Marked_As_Rm or this.Submit_Animation then
         declare
            Illegal_Newline : exception;
         begin
            raise Illegal_Newline with "Cannot submit newline for messages containing animation or marked as rm";
         end;
      end if;

      -- Stop animation if one is active
      if not this.Submit_Animation then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      -- Append whitespaces if the previous message was longer and marked as Rm
      Internal_Log(Get_Trailing_Whitespaces(this.Current_Message_Length, this.Last_Message_Length), this.Current_Message_Length);
      this.Last_Message_Length := 0; -- Reset because we have taken action

      File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), "" & Ada.Characters.Latin_1.LF);
      New_Line;

      -- Reset message length counter because we are now on a new line
      this.Current_Message_Length := 0; -- TODO: Test if this is correct

      return this;
   end Nl;


   -- Ends the message. This is a required dummy function as Ada forces us to process return values, which we don't want when being done calling Logger functions
   procedure EoL(this : access Logger_Dummy) is
   begin
      -- Append whitespaces if the previous message was longer and marked as Rm
      Internal_Log(Get_Trailing_Whitespaces(this.Current_Message_Length, this.Last_Message_Length), this.Current_Message_Length);
      this.Last_Message_Length := 0; -- Reset because we have taken action


      -- Check if the message was marked to be removed or contains an animation
      if this.Marked_As_Rm or this.Submit_Animation then

         -- Print a newline to the output file (nothing can be overwritten there) and a carriage return to stdout (so the next msg overwrites this one)
         File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), "" & Ada.Characters.Latin_1.LF);
         Put("" & Ada.Characters.Latin_1.CR);

         -- Start animation if one was set
         if this.Submit_Animation then
            Animation.Start(
               Animation_Frames => this.Current_Animation,
               Animation_Interval => this.Animate_Interval
            );
         end if;

         -- Save size so the next message can overwrite everything we've printed to avoid ghost chars
         this.Last_Message_Length := this.Current_Message_Length;

      else
         this.Last_Message_Length := 0; -- Reset because we have taken action
      end if;


      -- Reset size tracker of this message
      this.Current_Message_Length := 0;
   end EoL;

end Logger_Type;
