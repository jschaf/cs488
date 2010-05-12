--                              -*- Mode: Ada -*-
-- Filename        : turtle_graphics.ads
-- Description     : Simple turtle graphics for Ada using Gtk.
-- Author          : COL Ressler
-- Created On      : Sat Oct 04 22:15:59 2003
-- Last Modified By: COL Ressler
-- Last Modified On: .
-- Update Count    : 0
-- Status          : Initial implementation.
package Turtle_Graphics is

   -- Pop up a fresh window of the given size.  The turtle is
   -- initially in the middle.
   procedure Create_World(X_Max, Y_Max : in Float := 512.0);

   -- Clear the turtle window.
   procedure Clear;

   -- Have the turtle face in the given direction.  We are using the
   -- polar coordinate convention for angles, degrees counterclockwise
   -- from the x-axis.
   procedure Face(Theta : in Float);

   -- Adjust the direction that the turtle is facing by the given
   -- delta.  Positive values are left turns, negative values are
   -- right turns.
   procedure Turn(D_Theta : in Float);

   -- Fly straight forward the given distance in the direction the
   -- turtle is currently facing without making a trail.  A negative
   -- value causes backward movement.
   procedure Fly(Distance : in Float);

   -- Walk straight forward the given distance in the direction the
   -- turtle is currently facing, making an ink trail (line) from the
   -- old turtle location to the new.
   procedure Walk(Distance : in Float);

   -- Fly to a new coordinate without making a trail.  The direction
   -- that the turtle is facing is ignored and is not affected.
   procedure Fly(X, Y : in Float);

   -- Walk straight from the current turtle location to the given new
   -- one.  The direction that the turtle is facing is ignored and is
   -- not affected.
   procedure Walk(X, Y : in Float);

   -- Drop a spot of ink at the turtle's current location.
   procedure Spot(Diameter : in Float := 4.0);

   -- Draw a graphic justified at the turtle's current location.  This
   -- will draw any graphic that Gtk understands, including JPEG, GIF,
   -- BMP, PNG, PBM, TIFF, and perhaps some others.
   type X_Justification_Type is (Left, Center, Right);
   type Y_Justification_Type is (Top, Center, Bottom);
   procedure Spot(File_Name : in String;
                  Scale : in Float := 1.0;
                  X_Justification : in X_Justification_Type := Center;
                  Y_Justification : in Y_Justification_Type := Center);

   -- Draw a text string justified at the turtle's current location.
   -- The size is relative.
   procedure Scribble(Text : in String;
                      Size : in Float := 1.0;
                      X_Justification : in X_Justification_Type := Center;
                      Y_Justification : in Y_Justification_Type := Center);

   -- Save the current image for later Restore.
   procedure Save;

   -- Restore a previously saved image.  Do nothing if there has been no Save.
   procedure Restore;

   -- Pause the given number of seconds.  This also forces all the ink
   -- to be drawn immediately (even if duration is 0.0).  If you don't
   -- call Pause, the screen is updated only a few times per second.
   procedure Pause(Duration : in Float := 0.0);

   -- Close the turtle window and release all the resources used for
   -- drawing.
   procedure End_World;

   -- Raised by any of the above calls if someone has killed the
   -- turtle by closing his window or if the user never called
   -- Create_World to begin with.
   World_Does_Not_Exist : exception;

end Turtle_Graphics;
