-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-08-18 12:28:03
-- Modified By: 3urobeat
--
-- Copyright (c) 2024 3urobeat <https://github.com/3urobeat>
--
-- This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.
-- This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
-- You should have received a copy of the GNU Lesser General Public License along with this library. If not, see <https://www.gnu.org/licenses/>.


with Ada.Exceptions;
with Ada.IO_Exceptions;

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

   -- Starts a new log message
   function Logger return access Logger_Dummy is
      this : access Logger_Dummy := Logger_Instance'Access;
   begin
      -- Check if the last message should be reprinted
      if this.Submit_Animation and then not this.Marked_As_Rm then
         this.Submit_Animation := False;
         this.Internal_Log(Reprint_Bounded_512B.To_String(this.Animation_Reprint_Buffer)); -- Do not force New_Line, let user decide. If they call Nl(), it should handle overwriting ghost chars
         File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), Reprint_Bounded_512B.To_String(this.Animation_Reprint_Buffer));
      end if;

      -- Reset stuff
      this.Marked_As_Rm     := False;
      this.Submit_Animation := False;
      this.Animation_Reprint_Buffer := Reprint_Bounded_512B.Null_Bounded_String;

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
      -- Check if there is a running animation. If it is the same, get it printed and hold
      if this.Current_Animation = ANIM then
         this.Internal_Log(Animation.Log_Static); -- This prints the current animation frame once to offset the following message content
      else
         Animation.Stop;
         this.Internal_Log("[" & Animation_Frames_Bounded.To_String(ANIM(Animation_Index'First)) & "] ");
      end if;

      -- Register this animation
      this.Current_Animation := ANIM;
      this.Submit_Animation := True;   -- Make sure this is set after the initial frame was printed, so that Internal_Log() does not append the animation frame to the Reprint_Buffer

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
      this.Internal_Log(STR);

      return this;
   end Log;


   -- Logs a message to stdout with 'INFO' prefix
   function Info(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      this.Internal_Prefixed_Log(
         Log_Lvl  => "INFO",
         Color    => Colors.Br_Fg_Cyan,
         STR      => STR,
         SRC      => SRC,
         ND       => ND
      );

      return this;
   end Info;


   -- Logs a message to stdout with 'DEBUG' prefix
   function Debug(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      this.Internal_Prefixed_Log(
         Log_Lvl  => "DEBUG",
         Color    => Colors.Br_Fg_Cyan & Colors.Background,
         STR      => STR,
         SRC      => SRC,
         ND       => ND
      );

      return this;
   end Debug;


   -- Logs a message to stdout with 'WARN' prefix
   function Warn(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      this.Internal_Prefixed_Log(
         Log_Lvl  => "WARN",
         Color    => Colors.Fg_Red,
         STR      => STR,
         SRC      => SRC,
         ND       => ND
      );

      return this;
   end Warn;


   -- Logs a message to stdout with 'ERROR' prefix
   function Error(this : access Logger_Dummy; STR : String; SRC : String := ""; ND : Boolean := False) return access Logger_Dummy is
   begin
      this.Internal_Prefixed_Log(
         Log_Lvl  => "ERROR",
         Color    => Colors.Fg_Red & Colors.Background,
         STR      => Colors.Fg_Red & STR,
         SRC      => SRC,
         ND       => ND
      );

      return this;
   end Error;


   -- Logs a newline to stdout
   function Nl(this : access Logger_Dummy) return access Logger_Dummy is
   begin
      -- Deny newline call for messages awaiting carriage return as this would break overwriting it
      if this.Marked_As_Rm then
         declare
            Illegal_Newline : exception;
         begin
            raise Illegal_Newline with "Cannot submit newline for messages marked as rm";
         end;
      end if;

      -- Append newline (and trailing whitespaces) to reprint buffer if animation is active, otherwise stop any previously started animation
      if this.Submit_Animation then
         Reprint_Bounded_512B.Append(
            this.Animation_Reprint_Buffer,
            Get_Trailing_Whitespaces(
               Reprint_Bounded_512B.Length(this.Animation_Reprint_Buffer), -- Use content of reprint buffer as current length
               this.Current_Message_Length                                 -- ...and the current message length (containing the animation) as the last message length
            ) & Ada.Characters.Latin_1.LF       -- Append newline character to buffer
         );
         return this;
      else
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      -- Append whitespaces if the previous message was longer
      this.Internal_Log(Get_Trailing_Whitespaces(this.Current_Message_Length, this.Last_Message_Length));
      this.Last_Message_Length := 0; -- Reset because we have taken action

      File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), "" & Ada.Characters.Latin_1.LF);
      Print(Message, "" & Ada.Characters.Latin_1.LF);

      -- Reset message length counter because we are now on a new line
      this.Current_Message_Length := 0;

      return this;
   end Nl;


   -- Ends the message. This is a required dummy function as Ada forces us to process return values, which we don't want when being done calling Logger functions
   procedure EoL(this : access Logger_Dummy) is
   begin
      -- Append whitespaces if the previous message was longer and marked as Rm
      this.Internal_Log(Get_Trailing_Whitespaces(this.Current_Message_Length, this.Last_Message_Length));
      this.Last_Message_Length := 0; -- Reset because we have taken action


      -- Check if the message was marked to be removed or contains an animation
      if this.Marked_As_Rm or this.Submit_Animation then
         -- Print carriage return to stdout so the next msg overwrites this one
         Print(Print_Event_Type(Message), "" & Ada.Characters.Latin_1.CR);

         -- Always print newline to output file for messages marked as Rm because nothing can & should be overwritten there
         if this.Marked_As_Rm then
            File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), "" & Ada.Characters.Latin_1.LF);
         end if;

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


   -- Reads user input from stdin and returns it
   function Read_Input(this : access Logger_Dummy; Question : String := ""; Timeout : Duration := 0.0) return access String is

      -- This function frees me from declaring a constrained User_Input String
      function Get_User_Input return String is
         User_Input : aliased String := Get_Line;
      begin
         -- Hide cursor again and return result
         Put(Colors.Hide_Cursor);   -- Hide cursor again
         Unlock_Stdout;             -- Unlock Print_Manager to log queued messages
         return User_Input;
      end Get_User_Input;

   begin    -- TODO: Pause animations, cache new log calls (if even necessary), ...

      -- Print question if one was set and show cursor
      this.Internal_Log(Question); -- TODO: Should this use Internal_Log?  -- TODO: This does not appear in output file, is this intended?
      Put(Colors.Show_Cursor);

      -- Lock Print_Manager
      Lock_Stdout;

      -- Get input, abort if Timeout ran out (as long as a timeout was provided)
      if Timeout > 0.0 then
         select
            delay Timeout;
         then abort
            return new String'(Get_User_Input);
         end select;

         return null;
      else
         return new String'(Get_User_Input);
      end if;

   exception
      when ADA.IO_EXCEPTIONS.DEVICE_ERROR => -- Ignore Error caused by aborting Get_Line
         Put(Colors.Hide_Cursor);            -- Hide cursor again
         New_Line;                           -- Print New_Line because ENTER press by user is missing on abort
         Unlock_Stdout;                      -- Unlock Print_Manager to log queued messages
         return null;                        -- ...aaand return null
   end Read_Input;



   -- Internal: Logs a message as is to stdout
   procedure Internal_Log(this : in out Logger_Dummy; Str : String) is
   begin
      -- Check if message needs to be cut to terminal width
      if this.Marked_As_Rm or this.Submit_Animation then
         declare
            Cut_Str : String := Cut_To_Terminal_Width(Str, this.Current_Message_Length);
         begin
            If Cut_Str'Length > 0 then

               Print(Print_Event_Type(Message), Cut_Str);
               this.Current_Message_Length := this.Current_Message_Length + Cut_Str'Length;  -- TODO: This is not entirely accurate because it counts color codes

               -- Append to Reprint Buffer if we are in this block because an animation is contained
               if (not this.Marked_As_Rm) and then (Reprint_Bounded_512B.Length(this.Animation_Reprint_Buffer) + Cut_Str'Length <= Reprint_Bounded_512B.Max_Length) then
                  Reprint_Bounded_512B.Append(this.Animation_Reprint_Buffer, Cut_Str);
               end if;

            end if;
         end;
      else
         Print(Print_Event_Type(Message), Str);
         this.Current_Message_Length := this.Current_Message_Length + Str'Length;            -- TODO: This is not entirely accurate because it counts color codes
      end if;

      -- Always append Color Reset to avoid colors bleeding into the next element
      Print(Print_Event_Type(Message), Colors.Reset);
   end Internal_Log;


   -- Internal: Constructs the actual message and logs it to file & stdout
   procedure Internal_Prefixed_Log(this : in out Logger_Dummy; Log_Lvl : String; Color : String; STR : String; SRC : String := ""; ND : Boolean := False) is
      -- Construct string with prefix
      String_To_Log : String := Get_Prefix(Color, Log_Lvl, SRC, ND) & str;
   begin
      -- Stop animation from previous message
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      -- Only log to output file when message does not contain animation or is marked as Rm. Animation messages without Rm are reprinted in Logger() and will appear in output file
      if not this.Submit_Animation or this.Marked_As_Rm then
         File_Output.Print_To_File(Options_Bounded_128B.To_String(this.Output_File_Path), String_To_Log);
      end if;

      -- Let Internal_Log handle printing to stdout
      this.Internal_Log(String_To_Log);
   end Internal_Prefixed_Log;

end Logger_Type;
