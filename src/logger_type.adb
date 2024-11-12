-- File: logger_type.adb
-- Project: ada-output-logger
-- Created Date: 2024-06-30 13:01:43
-- Author: 3urobeat
--
-- Last Modified: 2024-11-12 10:06:51
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

   -- Create global Logger instance for everyone to use
   Logger_Instance : aliased Logger_Dummy;

   -- Starts a new log message
   function Logger(this : access Logger_Dummy) return access Logger_Dummy is
   begin
      -- Check if the last message should be reprinted
      if this.Submit_Animation and then not this.Marked_As_Rm then
         this.Submit_Animation := False;
         this.Internal_Log(Reprint_Bounded_512B.To_String(this.Animation_Reprint_Buffer)); -- Do not force New_Line, let user decide. If they call Nl(), it should handle overwriting ghost chars
         File_Output.Print_To_File(this.Output_File_Handle'Access, Options_Bounded_128B.To_String(this.Output_File_Path), Reprint_Bounded_512B.To_String(this.Animation_Reprint_Buffer));
      end if;

      -- Reset stuff
      this.Marked_As_Rm     := False;
      this.Submit_Animation := False;
      this.Animation_Reprint_Buffer := Reprint_Bounded_512B.Null_Bounded_String;

      return this;
   end Logger;


   -- Starts a new log message
   function Logger return access Logger_Dummy is
      this : access Logger_Dummy := Logger_Instance'Access;
   begin
      return Logger(this);
   end Logger;


   -- Marks this message to be overwritten by the next logger call
   function Rm(this : access Logger_Dummy) return access Logger_Dummy is
   begin
      this.Marked_As_Rm := True;

      return this;
   end Rm;


   -- Prepends the following message with an animation. The animation will be refreshed every  Call this before any
   function Animate(this : access Logger_Dummy; Anim : Animation_Type) return access Logger_Dummy is
   begin
      -- Check if there is a running animation. If it is the same, get it printed and hold
      if this.Current_Animation = Anim then
         this.Internal_Log(Animation.Log_Static); -- This prints the current animation frame once to offset the following message content
      else
         Animation.Stop;
         this.Internal_Log("[" & Animation_Frames_Bounded.To_String(Anim(Animation_Index'First)) & "] ");
      end if;

      -- Register this animation
      this.Current_Animation := Anim;
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


   -- Concat overload for Integer
   function "&"(Left : in String; Right : in Integer) return String is
   begin
      return Left & Ada.Strings.Fixed.Trim(Integer'Image(Right), Ada.Strings.Left);
   end "&";

   -- Concat overload for Float
   function "&"(Left : in String; Right : in Float) return String is
   begin
      return Left & Ada.Strings.Fixed.Trim(Float'Image(Right), Ada.Strings.Left);
   end "&";


   -- Logs a message to stdout without any formatting, use this for appending to an existing message
   function Log(this : access Logger_Dummy; Msg : String) return access Logger_Dummy is
   begin
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      File_Output.Print_To_File(this.Output_File_Handle'Access, Options_Bounded_128B.To_String(this.Output_File_Path), Msg);
      this.Internal_Log(Msg);

      return this;
   end Log;

   function Log(this : access Logger_Dummy; Msg : Integer) return access Logger_Dummy is
   begin
      return this.Log(Msg => "" & Msg);
   end Log;

   function Log(this : access Logger_Dummy; Msg : Float) return access Logger_Dummy is
   begin
      return this.Log(Msg => "" & Msg);
   end Log;


   -- Logs a String to stdout, prefixed with log level, source & date (all optional)
   function Log(this : access Logger_Dummy; Lvl : Log_Levels; Msg : String; Src : String := ""; Nd : Boolean := False) return access Logger_Dummy is
   begin
      case Lvl is
         when Debug =>
            this.Internal_Prefixed_Log(
               Log_Lvl  => "DEBUG",
               Color    => Colors.Br_Fg_Cyan & Colors.Background,
               Msg      => Msg,
               Src      => Src,
               Nd       => Nd
            );

         when Info =>
            this.Internal_Prefixed_Log(
               Log_Lvl  => "INFO",
               Color    => Colors.Br_Fg_Cyan,
               Msg      => Msg,
               Src      => Src,
               Nd       => Nd
            );

         when Warn =>
            this.Internal_Prefixed_Log(
               Log_Lvl  => "WARN",
               Color    => Colors.Fg_Red,
               Msg      => Msg,
               Src      => Src,
               Nd       => Nd
            );

         when Error =>
            this.Internal_Prefixed_Log(
               Log_Lvl  => "ERROR",
               Color    => Colors.Fg_Red & Colors.Background,
               Msg      => Colors.Fg_Red & Msg,
               Src      => Src,
               Nd       => Nd
            );

         when others =>
            this.Internal_Prefixed_Log(
               Log_Lvl  => "",
               Color    => "",
               Msg      => Msg,
               Src      => Src,
               Nd       => Nd
            );

      end case;

      return this;
   end Log;

   function Log(this : access Logger_Dummy; Lvl : Log_Levels; Msg : Integer; Src : String := ""; Nd : Boolean := False) return access Logger_Dummy is
   begin
      return this.Log(Lvl => Lvl, Msg => "" & Msg, Src => Src, Nd => Nd);
   end Log;

   function Log(this : access Logger_Dummy; Lvl : Log_Levels; Msg : Float; Src : String := ""; Nd : Boolean := False) return access Logger_Dummy is
   begin
      return this.Log(Lvl => Lvl, Msg => "" & Msg, Src => Src, Nd => Nd);
   end Log;


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

      File_Output.Print_To_File(this.Output_File_Handle'Access, Options_Bounded_128B.To_String(this.Output_File_Path), "" & Ada.Characters.Latin_1.LF);
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
            File_Output.Print_To_File(this.Output_File_Handle'Access, Options_Bounded_128B.To_String(this.Output_File_Path), "" & Ada.Characters.Latin_1.LF);
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

   begin

      -- New Logger instance to reprint anything, Rm to avoid Question being offset, Log to stop animation, EoL to overwrite ghost chars
      Logger.Rm.Log("").EoL;

      -- Lock Print_Manager nearly instantly to avoid glitching messages in concurrent applications
      Lock_Stdout;

      -- Print question if one was set, add additional newline to output file and show cursor
      if Question'Length > 0 then
         Print(Event => Read_Input_Start, Str => Question);  -- Print directly via the Print_Manager to bypass lock
         File_Output.Print_To_File(this.Output_File_Handle'Access, Options_Bounded_128B.To_String(this.Output_File_Path), Question & Ada.Characters.Latin_1.LF);
      end if;

      Put(Colors.Show_Cursor);


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
   procedure Internal_Prefixed_Log(this : in out Logger_Dummy; Log_Lvl : String; Color : String; Msg : String; Src : String := ""; Nd : Boolean := False) is
      -- Construct string with prefix
      String_To_Log : String := Get_Prefix(Color, Log_Lvl, Src, Nd) & Msg;
   begin
      -- Stop animation from previous message
      if this.Submit_Animation = False then
         Animation.Stop;
         this.Current_Animation := Default_Animations.None;
      end if;

      -- Only log to output file when message does not contain animation or is marked as Rm. Animation messages without Rm are reprinted in Logger() and will appear in output file
      if not this.Submit_Animation or this.Marked_As_Rm then
         File_Output.Print_To_File(this.Output_File_Handle'Access, Options_Bounded_128B.To_String(this.Output_File_Path), String_To_Log);
      end if;

      -- Let Internal_Log handle printing to stdout
      this.Internal_Log(String_To_Log);
   end Internal_Prefixed_Log;


   -- Internal: Logs exit message and shows cursor
   procedure Ada_Log_Exit is
      Exit_Msg : String := Options_Bounded_128B.To_String(Logger_Instance.Exit_Message);
   begin
      Print(Print_Event_Type(Finalize), Colors.Show_Cursor);

      -- Disable printing Exit_Msg to output file for now, it always causes a Runtime Exception probably because the file handle is already closed
      Print(Print_Event_Type(Finalize), Exit_Msg & Ada.Characters.Latin_1.LF);
      --  Logger.Log(Exit_Msg).Nl.EoL;
   end Ada_Log_Exit;


   -- Internal: Implemented in C to setup signal handler
   procedure C_Handle_Exit with
      Import => True,
      Convention => C,
      External_Name => "c_exit_handler";

begin

   -- Attach signal interrupt handlers and hide cursor once when the library is initially loaded
   C_Handle_Exit;

   Put(Colors.Hide_Cursor);

end Logger_Type;
