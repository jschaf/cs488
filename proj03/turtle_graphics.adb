--                              -*- Mode: Ada -*-
-- Filename        : turtle_graphics.adb
-- Description     : Turtle graphics for Ada with AdaGtk
-- Author          : COL Gene Ressler
-- Created On      : Thu Jul 21 12:02:15 2005
-- Last Modified By: COL Ressler
-- Last Modified On: 24 Feb 2007
-- Update Count    : 6
-- Status          : Production.
-- Version 1.00 Initial
-- Version 1.01 Poll user task and terminate if it does.
-- Version 1.02 Justification for image spots.
-- Version 1.03 Justified text with Scribble.
-- Version 1.04 Added Save/Restore for MIEDS simulator.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Task_Identification;           use Ada.Task_Identification;
with Ada.Containers;                    use Ada.Containers;
with Unchecked_Deallocation, Ada.Containers.Hashed_Maps;

with Glib;             use Glib;
with Glib.Error;       use Glib.Error;
with Gdk;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Rgb;          use Gdk.Rgb;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Window;       use Gdk.Window;
with Gtk;              use Gtk;
with Gtk.Box;          use Gtk.Box;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;         use Gtk.Main;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Style;        use Gtk.Style;
with Gtk.Window;       use Gtk.Window;
with Gdk.Font;         use Gdk.Font;

with Queues;

package body Turtle_Graphics is

   -- Commands are records in a protected queue.  The user task is the
   -- producer of commands, and our own Main_Exec task is the
   -- consumer.  We could have used a tagged type and dispatching
   -- command execution.

   -----------------------
   -- Kinds of commands --
   -----------------------
   type Cmd_Kind_Type is (No_Op, Clear,
                          Face, Turn,
                          Fly_Forward, Walk_Forward,
                          Fly_To, Walk_To,
                          Ink_Spot, Img_Spot, Scribble,
                          Save, Restore,
                          Pause, End_It);

   ---------------------------------------------------------------
   -- Strings for graphic file names in the image spot command. --
   ---------------------------------------------------------------
   type String_Ptr_Type is access String;

   procedure Free_String is
     new Unchecked_Deallocation(String, String_Ptr_Type);

   ---------------------
   -- Command records --
   ---------------------
   type Cmd_Type(Kind : Cmd_Kind_Type := No_Op) is
      record
         case Kind is
            when No_Op | Clear | Save | Restore | End_It =>
               null;
            when Face =>
               Theta : Float;
            when Turn =>
               D_Theta : Float;
            when Fly_Forward | Walk_Forward  =>
               Distance : Float;
            when Fly_To | Walk_To =>
               X, Y : Float;
            when Ink_Spot =>
               Diameter : Float;
            when Img_Spot | Scribble =>
               X_Justification : X_Justification_Type;
               Y_Justification : Y_Justification_Type;
               case Kind is
                  when Img_Spot =>
                     File_Name : String_Ptr_Type;
                     Scale : Float;
                  when Scribble =>
                     Text : String_Ptr_Type;
                     Size : Float;
                  when others =>
                     null;
               end case;
            when Pause =>
               Duration : Float;
         end case;
      end record;

   -------------------
   -- Command queue --
   -------------------
   package Cmd_Queues is
      new Queues(Element_Type => Cmd_Type);

   Cmd_Queue : Cmd_Queues.Protected_Queue_Type;

   ---------------------------------------------------
   -- Gtk globals for communication among callbacks --
   ---------------------------------------------------
   User_Task    : Task_Id := Null_Task_ID; -- Task that sets up world.
   Pixmap       : Gdk_Pixmap;           -- Pixmap used as backing store.
   Save_Pixmap  : Gdk_Pixmap;           -- Save/Restore command pixmap.
   Window       : Gtk_Window;
   Pausing      : Boolean := False;
   Vbox         : Gtk_Box;
   Drawing_Area : Gtk_Drawing_Area;
   Idle_Function_Id : Idle_Handler_Id;
   Pause_Timeout_Function_Id : Timeout_Handler_Id;
   Draw_Timeout_Function_Id : Timeout_Handler_Id;
   Width, Height : Gint;

   ------------
   -- Turtle --
   ------------
   Turtle_X, Turtle_Y : Float := 0.0;
   Turtle_Theta : Float := 0.0;

   package Configured is
      new Gtk.Handlers.Return_Callback(Widget_Type => Gtk_Drawing_Area_Record,
                                       Return_Type => Boolean);
   package Destroyed is
      new Gtk.Handlers.Callback(Widget_Type => Gtk_Window_Record);

   -- Dirty mark to be set true if something has been drawn on the
   -- backing store pixmap that needs to be copied to the screen the
   -- next time Draw_If_Dirty is called.
   Drawing_Area_Dirty : Boolean := True;

   procedure Draw_If_Dirty is
   begin
      if Drawing_Area_Dirty then
         Draw (Drawing_Area);
         Drawing_Area_Dirty := False;
      end if;
   end Draw_If_Dirty;

   ---------------------
   -- Configure_Event --
   ---------------------

   function Configure_Event(Drawing_Area : access Gtk_Drawing_Area_Record'Class)
                           return Boolean is
      Win    : Gdk_Window;
      New_Width  : Gint;
      New_Height : Gint;
      New_Pixmap : Gdk_Pixmap;
      use type Gdk.Gdk_Drawable;
   begin
      Win := Get_Window (Drawing_Area);

      -- Allocate a new pixmap of the reconfigured size and clear it to white.
      Get_Size (Win, New_Width, New_Height);
      Gdk.Pixmap.Gdk_New (New_Pixmap, Win, New_Width, New_Height, -1);
      Draw_Rectangle (New_Pixmap, Get_White (Get_Style (Drawing_Area)),
                      True, 0, 0, New_Width, New_Height);

      -- If there was a pixmap previously, copy the image to the new
      -- one.  Free the original.
      if Pixmap /= Null_Pixmap then
         Draw_Pixmap(New_Pixmap,
                     Get_Fg_GC (Get_Style (Drawing_Area), State_Normal),
                     Pixmap, 0, 0, 0, 0,
                     Gint'Min(Width, New_Width), Gint'Min(Height, New_Height));
         Gdk.Pixmap.Unref(Pixmap);
      end if;

      -- Update globals.
      Pixmap := New_Pixmap;
      Height := New_Height;
      Width := New_Width;
      return True;
   end Configure_Event;

   ------------------
   -- Expose_Event --
   ------------------

   function Expose_Event
     (Drawing_Area : access Gtk_Drawing_Area_Record'Class;
      Event : in Gdk.Event.Gdk_Event)
     return Boolean
   is
      Area : Gdk_Rectangle := Get_Area (Event);
   begin
      -- Restore screen from backing store pixmap.
      Draw_Pixmap (Get_Window (Drawing_Area),
                   Get_Fg_GC (Get_Style (Drawing_Area), State_Normal),
                   Pixmap, Area.X, Area.Y, Area.X, Area.Y,
                   Gint (Area.Width), Gint (Area.Height));
      return True;
   end Expose_Event;

   ---------
   -- Bye --
   ---------
   procedure Bye (Window : access Gtk.Window.Gtk_Window_Record'Class) is
      pragma Warnings (Off, Window);
   begin
      Gtk.Main.Main_Quit;
   end Bye;

   -----------------------
   -- Command executors --
   -----------------------
   procedure Do_Clear(Cmd : in out Cmd_Type) is
      pragma Warnings (Off, Cmd);
      Win    : Gdk_Window;
      Width  : Gint;
      Height : Gint;
   begin
      Win := Get_Window (Drawing_Area);
      Get_Size (Win, Width, Height);
      Draw_Rectangle (Pixmap, Get_White (Get_Style (Drawing_Area)),
                      True, 0, 0, Width, Height);
      Drawing_Area_Dirty := True;
   end Do_Clear;

   procedure Do_Ink_Spot(Cmd : in out Cmd_Type) is
      R : Gint := Gint(Cmd.Diameter/2.0);
   begin
      Draw_Arc (Pixmap, Get_Black (Get_Style (Drawing_Area)), True,
                Gint(Turtle_X) - R, Height - Gint(Turtle_Y) - R, 2 * R, 2 * R, 0, 360 * 64);
      Drawing_Area_Dirty := True;
   end Do_Ink_Spot;

   type Cached_Image_Type is
      record
         File_Name : String_Ptr_Type;
         Scale : Float;
         Cooked_Image_Pixbuf, Raw_Image_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      end record;

   function "=" (A, B : String_Ptr_Type) return Boolean is
   begin
      return A.all = B.all;
   end "=";

   function "=" (A, B : Cached_Image_Type) return Boolean is
   begin
      return A.File_Name = B.File_Name;
   end "=";

   function Hash(S : in String_Ptr_Type) return Hash_Type is
      Val : Hash_Type := 0;
   begin
      for I in S'Range loop
         Val := Val * 2 ** 4 + Character'Pos(S(I));
         Val := Val xor ((Val / (2 ** 24)) and 16#F0#);
      end loop;
      return Val;
   end Hash;

   package Image_Caches is
     new Ada.Containers.Hashed_Maps(String_Ptr_Type,
                                    Cached_Image_Type,
                                    Hash, "=", "=");
   use Image_Caches;
   subtype Image_Cashe_Type is Image_Caches.Map;

   Image_Cache : Image_Cashe_Type;

   procedure Do_Img_Spot(Cmd : in out Cmd_Type) is

      Read_Failed : Exception;

      procedure Read_Image(Pixbuf : in out Gdk.Pixbuf.Gdk_Pixbuf) is
         Error : GError;
      begin
         Gdk_New_From_File(Pixbuf, Cmd.File_Name.all, Error);
         if Error /= null then
            Put_Line ("Error getting image: " & Get_Message (Error));
            Error_Free (Error);
            raise Read_Failed;
         end if;
      end Read_Image;

      function Scale(Dimension : in Gint) return Gint is
      begin
         return Gint(Float(Dimension) * Cmd.Scale);
      end Scale;

      Cooked_Image_Pixbuf, Raw_Image_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;

      procedure Process(Key : in String_Ptr_Type;
                        Cached_Image : in out Cached_Image_Type) is
      begin
         if Cmd.Scale = Cached_Image.Scale then
            Cooked_Image_Pixbuf := Cached_Image.Cooked_Image_Pixbuf;
            Raw_Image_Pixbuf := Cached_Image.Raw_Image_Pixbuf;
         elsif Cmd.Scale = 1.0 then
            Unref(Cached_Image.Cooked_Image_Pixbuf);
            Cooked_Image_Pixbuf := Cached_Image.Raw_Image_Pixbuf;
            Cached_Image.Cooked_Image_Pixbuf := Cooked_Image_Pixbuf;
            Unref(Cached_Image.Raw_Image_Pixbuf);
            Cached_Image.Scale := 1.0;
         else
            Cooked_Image_Pixbuf := Scale_Simple(Cached_Image.Raw_Image_Pixbuf,
                                                Scale(Get_Width(Raw_Image_Pixbuf)),
                                                Scale(Get_Height(Raw_Image_Pixbuf)));
            Cached_Image.Cooked_Image_Pixbuf := Cooked_Image_Pixbuf;
            Unref(Cached_Image.Raw_Image_Pixbuf);
            Cached_Image.Scale := Cmd.Scale;
         end if;
      end Process;

      X_Pix, Y_Pix : Gint;
      C : Cursor := Find(Image_Cache, Cmd.File_Name);
   begin
      if C = No_Element then
         if Cmd.Scale = 1.0 then
            Read_Image(Cooked_Image_Pixbuf);
         else
            Read_Image(Raw_Image_Pixbuf);
            Cooked_Image_Pixbuf :=
              Scale_Simple(Raw_Image_Pixbuf,
                           Scale(Get_Width(Raw_Image_Pixbuf)),
                           Scale(Get_Height(Raw_Image_Pixbuf)));
         end if;
         Insert(Image_Cache, Cmd.File_Name,
           Cached_Image_Type'(File_Name => Cmd.File_Name,
                              Scale => Cmd.Scale,
                              Cooked_Image_Pixbuf => Cooked_Image_Pixbuf,
                              Raw_Image_Pixbuf => Raw_Image_Pixbuf));
      else
         Update_Element(Image_Cache, C, Process'Access);
         Free_String(Cmd.File_Name);
      end if;

      case Cmd.X_Justification is
         when Left =>
            X_Pix := Gint(Turtle_X);
         when Center =>
            X_Pix := Gint(Turtle_X) - Get_Width(Cooked_Image_Pixbuf) / 2;
         when Right =>
            X_Pix := Gint(Turtle_X) - Get_Width(Cooked_Image_Pixbuf);
      end case;
      case Cmd.Y_Justification is
         when Top =>
            Y_Pix := Height - Gint(Turtle_Y);
         when Center =>
            Y_Pix := Height - Gint(Turtle_Y) - Get_Height(Cooked_Image_Pixbuf) / 2;
         when Bottom =>
            Y_Pix := Height - Gint(Turtle_Y) - Get_Height(Cooked_Image_Pixbuf);
      end case;
      Render_To_Drawable(Cooked_Image_Pixbuf,
                         Pixmap,
                         Get_Black_GC (Get_Style (Drawing_Area)),
                         0, 0, -- source
                         X_Pix, Y_Pix, -- upper left corner
                         Get_Width(Cooked_Image_Pixbuf), Get_Height(Cooked_Image_Pixbuf),
                         Dither_Normal,
                         0, 0); -- dither offset
      Drawing_Area_Dirty := True;
   end Do_Img_Spot;

   procedure Do_Scribble(Cmd : in out Cmd_Type) is
      Font : Gdk_Font;
      Pt_Size : Natural := Natural(Cmd.Size * 160.0);
      Pt_Size_Str : String := Pt_Size'Img;
      X_Pix, Y_Pix, Lb, Rb, Width, Asc, Desc : Gint;
   begin
      Load(Font,
           "-*-arial-*-r-*-*-*-" &
           Pt_Size_Str(2..Pt_Size_Str'Last) &
           "-*-*-*-*-*-*");
      String_Extents(Font, Cmd.Text.all, Lb, Rb, Width, Asc, Desc);
      case Cmd.X_Justification is
         when Left =>
            X_Pix := Gint(Turtle_X);
         when Center =>
            X_Pix := Gint(Turtle_X) - Width / 2;
         when Right =>
            X_Pix := Gint(Turtle_X) - Width;
      end case;
      case Cmd.Y_Justification is
         when Bottom =>
            Y_Pix := Height - Gint(Turtle_Y) - Desc;
         when Center =>
            Y_Pix := Height - Gint(Turtle_Y) + Asc / 2;
         when Top =>
            Y_Pix := Height - Gint(Turtle_Y) + Asc;
      end case;
      Draw_Text(Drawable    => Pixmap,
                Font        => Font,
                Gc          => Get_Black_GC (Get_Style (Drawing_Area)),
                X           => X_Pix,
                Y           => Y_Pix,
                Text        => Cmd.Text.all);
      Drawing_Area_Dirty := True;
      Free_String(Cmd.Text);
   end Do_Scribble;

   procedure Do_Save(Cmd : in out Cmd_Type) is
      use type Gdk.Gdk_Drawable;
      Win    : Gdk_Window;
      Width  : Gint;
      Height : Gint;
   begin
      if Save_Pixmap /= Null_Pixmap then
         Gdk.Pixmap.Unref(Save_Pixmap);
      end if;
      Win := Get_Window (Drawing_Area);
      Get_Size (Win, Width, Height);
      Gdk.Pixmap.Gdk_New (Save_Pixmap, Get_Window(Drawing_Area), Width, Height, -1);
      Draw_Pixmap(Save_Pixmap,
                  Get_Fg_GC (Get_Style (Drawing_Area), State_Normal),
                  Pixmap, 0, 0, 0, 0, Width, Height);
   end Do_Save;

   procedure Do_Restore(Cmd : in out Cmd_Type) is
      use type Gdk.Gdk_Drawable;
      Win    : Gdk_Window;
      Width  : Gint;
      Height : Gint;
   begin
      if Save_Pixmap /= Null_Pixmap then
         Win := Get_Window (Drawing_Area);
         Get_Size (Win, Width, Height);
         Draw_Pixmap(Pixmap,
                     Get_Fg_GC (Get_Style (Drawing_Area), State_Normal),
                     Save_Pixmap, 0, 0, 0, 0, Width, Height);
         Drawing_Area_Dirty := True;
      end if;
   end Do_Restore;

   procedure Do_Face(Cmd : in out Cmd_Type) is
   begin
      Turtle_Theta := Cmd.Theta;
   end Do_Face;

   procedure Do_Turn(Cmd : in out Cmd_Type) is
   begin
      Turtle_Theta := Turtle_Theta + Cmd.D_Theta;
   end Do_Turn;

   procedure Do_Fly_Forward(Cmd : in out Cmd_Type) is
   begin
      Turtle_X := Turtle_X + Cmd.Distance * Cos(Turtle_Theta, 360.0);
      Turtle_Y := Turtle_Y + Cmd.Distance * Sin(Turtle_Theta, 360.0);
   end Do_Fly_Forward;

   procedure Do_Walk_Forward(Cmd : in out Cmd_Type) is
      Old_Turtle_X : constant Float := Turtle_X;
      Old_Turtle_Y : constant Float := Turtle_Y;
   begin
      Do_Fly_Forward(Cmd);
      Draw_Line(Pixmap, Get_Black_GC (Get_Style (Drawing_Area)),
                Gint(Old_Turtle_X), Height - Gint(Old_Turtle_Y),
                Gint(Turtle_X), Height - Gint(Turtle_Y));
      Drawing_Area_Dirty := True;
   end Do_Walk_Forward;

   procedure Do_Fly_To(Cmd : in out Cmd_Type) is
   begin
      Turtle_X := Cmd.X;
      Turtle_Y := Cmd.Y;
   end Do_Fly_To;

   procedure Do_Walk_To(Cmd : in out Cmd_Type) is
      Old_Turtle_X : constant Float := Turtle_X;
      Old_Turtle_Y : constant Float := Turtle_Y;
   begin
      Do_Fly_To(Cmd);
      Draw_Line(Pixmap, Get_Black_GC (Get_Style (Drawing_Area)),
                Gint(Old_Turtle_X), Height - Gint(Old_Turtle_Y),
                Gint(Turtle_X), Height - Gint(Turtle_Y));
      Drawing_Area_Dirty := True;
   end Do_Walk_To;

   function Pause_Timeout_Function return Boolean is
   begin
      Pausing := False;
      return False;
   end Pause_Timeout_Function;

   function Draw_Timeout_Function return Boolean is
   begin
      Draw_If_Dirty;
      return True;
   end Draw_Timeout_Function;

   procedure Do_Pause(Cmd : in out Cmd_Type) is
   begin
      -- Kill the timeout drawing function and draw right now.
      Timeout_Remove(Draw_Timeout_Function_Id);
      Draw_If_Dirty;
      if Cmd.Duration > 0.0 then
         Pausing := True;
         Pause_Timeout_Function_Id :=
           Timeout_Add(Guint32(Cmd.Duration * 1000.0),
                       Pause_Timeout_Function'Access);
      else
         -- Restart fresh timeout.
         Draw_Timeout_Function_Id := Timeout_Add(200, Draw_Timeout_Function'Access);
      end if;
   end Do_Pause;

   procedure Do_End_It(Cmd : in out Cmd_Type) is
      pragma Warnings(Off, Cmd);
   begin
      Gtk.Main.Main_Quit;
   end Do_End_It;

   ---------------------------------------------------------------------------
   -- Idle function pulls commands from queue when nothing else is going on --
   ---------------------------------------------------------------------------
   function Idle_Function return Boolean is
      Cmd : Cmd_Type;

      -- Roll our own dispatch table!
      type Do_Cmd_Proc_Type is access procedure (Cmd : in out Cmd_Type);
      type Do_Cmd_Table_Type is array(Cmd_Kind_Type) of Do_Cmd_Proc_Type;
      Do_Cmd : constant Do_Cmd_Table_Type :=
        (No_Op        => null,
         Clear        => Do_Clear'Access,
         Face         => Do_Face'Access,
         Turn         => Do_Turn'Access,
         Fly_Forward  => Do_Fly_Forward'Access,
         Walk_Forward => Do_Walk_Forward'Access,
         Fly_To       => Do_Fly_To'Access,
         Walk_To      => Do_Walk_To'Access,
         Ink_Spot     => Do_Ink_Spot'Access,
         Img_Spot     => Do_Img_Spot'Access,
         Scribble     => Do_Scribble'Access,
         Save         => Do_Save'Access,
         Restore      => Do_Restore'Access,
         Pause        => Do_Pause'Access,
         End_It       => Do_End_It'Access);
   begin
      if Pausing then
         delay 0.01; -- Be nice. Yield processor.
      else
         if Cmd_Queue.Length = 0 then
            IF Is_Callable(User_Task) then
               Draw_If_Dirty;
            else
               Gtk.Main.Main_Quit;
            end if;
         else
            Cmd_Queue.Dequeue(Cmd);
            if Do_Cmd(Cmd.Kind) /= null Then
               Do_Cmd(Cmd.Kind).all(Cmd);
            end if;
         end if;
      end if;
      return True;
   end Idle_Function;

   -----------------------------------------------
   -- Main executive task to run the event loop --
   -----------------------------------------------

   task type Gtk_Main_Exec_Type;
   type Gtk_Main_Exec_Ptr_Type is access all Gtk_Main_Exec_Type;
   Main_Exec : Gtk_Main_Exec_Ptr_Type;

   task body Gtk_Main_Exec_Type is
   begin
      Gtk.Main.Init;
      Gtk_New (Window, Window_Toplevel);
      Set_Title (Window, "Turtle World");
      Set_Border_Width (Window, Border_Width => 5);
      Destroyed.Connect (Window, "destroy", Destroyed.To_Marshaller (Bye'Access));

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Window, Vbox);
      Show (Vbox);

      Gtk_New (Drawing_Area);
      Size (Drawing_Area, Width, Height);
      Pack_Start (In_Box => Vbox, Child => Drawing_Area);

      Set_Events (Drawing_Area, Exposure_Mask);

      Configured.Connect (Widget => Drawing_Area,
                          Name   => "expose_event",
                          Marsh  => Configured.To_Marshaller
                            (Expose_Event'Access));

      Configured.Connect (Widget => Drawing_Area,
                          Name   => "configure_event",
                          Marsh  => Configured.To_Marshaller
                            (Configure_Event'Access));

      Show_All (Window);
      Idle_Function_Id := Idle_Add(Idle_Function'Access);
      Draw_Timeout_Function_Id := Timeout_Add(200, Draw_Timeout_Function'Access);

      Gtk.Main.Main;

      -- This code executes after a End_It call, if someone closes the
      -- turtle window or something goes really wrong in one of the
      -- callbacks.  Send a crude signal that we're terminating to
      -- Check_Main_Exec.  Then clear command queue to allow tasks
      -- blocked on a full queue to (perhaps) call check_main_exec and
      -- therefore receive the World_Ended exception.  This is a
      -- memory leak.
      Main_Exec := null;
      Cmd_Queue.Clear;
   end;

   -- If the main loop task terminated, raise an exception in the user
   -- task.
   procedure Check_Main_Exec is
   begin
      if Main_Exec = null then
         raise World_Does_Not_Exist;
      end if;
   end Check_Main_Exec;

   -----------------------------
   -- User task entry points. --
   -----------------------------

   -- Create a fresh world.
   procedure Create_World(X_Max, Y_Max : in Float := 512.0) is
   BEGIN
      IF Main_Exec = NULL then
         Width := Gint(X_Max);
         Height := Gint(Y_Max);

         -- Put the turtle in the middle of his world.
         Turtle_X := Float(Width) / 2.0;
         Turtle_Y := Float(Height) / 2.0;

         -- Get the id of the user's task.
         User_Task := Current_Task;

         -- Create the main Gtk loop task.
         Main_Exec := new Gtk_Main_Exec_Type;
      ELSE
         Put_Line(Current_Error, "Warning: Create_World was called when world already exists.");
      END IF;
   end Create_World;

   procedure Clear is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Clear));
   end Clear;

   procedure Face(Theta : in Float) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Face, Theta => Theta));
   end Face;

   procedure Turn(D_Theta : in Float) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Turn, D_Theta => D_Theta));
   end Turn;

   procedure Fly(Distance : in Float) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Fly_Forward, Distance => Distance));
   end Fly;

   procedure Walk(Distance : in Float) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Walk_Forward, Distance => Distance));
   end Walk;

   procedure Fly(X, Y : in Float) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Fly_To, X => X, Y => Y));
   end Fly;

   procedure Walk(X, Y : in Float) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Walk_To, X => X, Y => Y));
   end Walk;

   procedure Spot(Diameter : in Float := 4.0) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Ink_Spot, Diameter => Diameter));
   end Spot;

   procedure Spot(File_Name : in String;
                  Scale : in Float := 1.0;
                  X_Justification : in X_Justification_Type := Center;
                  Y_Justification : in Y_Justification_Type := Center) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Img_Spot,
                                  File_Name => new String'(File_Name),
                                  Scale => Scale,
                                  X_Justification => X_Justification,
                                  Y_Justification => Y_Justification));
   end Spot;

   procedure Scribble(Text : in String;
                      Size : in Float := 1.0;
                      X_Justification : in X_Justification_Type := Center;
                      Y_Justification : in Y_Justification_Type := Center) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Scribble,
                                  Text => new String'(Text),
                                  Size => Size,
                                  X_Justification => X_Justification,
                                  Y_Justification => Y_Justification));
   end Scribble;

   procedure Save is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Save));
   end Save;

   procedure Restore is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Restore));
   end Restore;

   procedure Pause(Duration : in Float := 0.0) is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => Pause, Duration => Float'Max(0.0, Duration)));
   end Pause;

   procedure End_World is
   begin
      Check_Main_Exec;
      Cmd_Queue.Enqueue(Cmd_Type'(Kind => End_It));
   end End_World;

begin
   Cmd_Queue.Set_Size_Limit(100);
end Turtle_Graphics;
