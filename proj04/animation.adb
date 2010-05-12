with AST, String_Table, Turtle_Graphics;
use  AST, String_Table, Turtle_Graphics;

package body Animation is

   ----------------
   -- Initialize --
   ----------------

   -- The Viewport is the Turtlegraphics Window.  Units are pixels.
   Viewport_X_Size : constant := 960.0;
   Viewport_Y_Size : constant := 960.0;
   X_Margin : constant := 32.0;
   Y_Margin : constant := 32.0;

   -- The world is the MIEDS simulation scenario.  These are set
   -- dynamically to fit the scenario.  Initial values ensure min- and
   -- max-finding work correctly.
   World_X_Min : Real := Real'Safe_Last;
   World_X_Max : Real := Real'Safe_First;
   World_Y_Min : Real := Real'Safe_Last;
   World_Y_Max : Real := Real'Safe_First;

   -- True iff graphical output is enabled.
   Enabled_P : Boolean := True;

   -- True iff Turtlegraphics has been initialized.
   Initialized_P : Boolean := False;

   -- Enable/disable graphical output.
   procedure Enable(Val : in Boolean := True) is
   begin
      Enabled_P := Val;
   end Enable;

   -- Convert a world coordinate point into Turtlegraphics coordinates.
   procedure World_To_Viewport(From : in Point_2d_Type;
                               To_X, To_Y : out Float) is
   begin
      To_X := X_Margin + Viewport_X_Size *
        Float((From.X - World_X_Min) / (World_X_Max - World_X_Min));
      To_Y := Y_Margin + Viewport_Y_Size *
        Float((From.Y - World_Y_Min) / (World_Y_Max - World_Y_Min));
   end World_To_Viewport;

   -- Initialize the graphics environment.  Must be enabled for this to work.
   procedure Initialize(State : in Simulation_State_Type) is
      use Node_Lists;
      C : Cursor;
      Description : Node_Ptr_Type;
      Gp : Geopoint_Ptr_Type;
      Point : Point_2d_Type;
      Ax, Ay, Bx, By : Float;
   begin

      if not Enabled_P or Initialized_P then
         return;
      end if;

      Initialized_P := True;

      -- Initialize turtlegraphics.
      Create_World(Viewport_X_Size + 2.0 * X_Margin,
                   Viewport_Y_Size + 2.0 * Y_Margin);

      -- We are going to draw the background and then save it for use in
      -- successive frames.

      -- Run through geopoints to compute the extent of the world.
      C := First(State.Model.Defs);
      while C /= No_Element loop
         Description := Def_Lambda_Description(Element(C));
         if Description.all in Geopoint_Type then
            Point := Value_Of(Geopoint_Ptr_Type(Description));
            World_X_Min := Real'Min(World_X_Min, Point.X);
            World_X_Max := Real'Max(World_X_Max, Point.X);
            World_Y_Min := Real'Min(World_Y_Min, Point.Y);
            World_Y_Max := Real'Max(World_Y_Max, Point.Y);
         end if;
         C := Next(C);
      end loop;

      -- Draw all the road segments.
      C := First(State.Model.Defs);
      while C /= No_Element loop
         Description := Def_Lambda_Description(Element(C));
         if Description.all in Segment_Type then
            Gp := Geopoint_Ptr_Type(Segment_Ptr_Type(Description).A);
            World_To_Viewport(Value_Of(Gp), Ax, Ay);
            Gp := Geopoint_Ptr_Type(Segment_Ptr_Type(Description).B);
            World_To_Viewport(Value_Of(Gp), Bx, By);
            Fly(Ax, Ay);
            Walk(Bx, By);
         end if;
         C := Next(C);
      end loop;

      -- Draw and label all the geopoints.
      C := First(State.Model.Defs);
      while C /= No_Element loop
         Description := Def_Lambda_Description(Element(C));
         if Description.all in Geopoint_Type then
            Gp := Geopoint_Ptr_Type(Description);
            World_To_Viewport(Value_Of(Gp), Ax, Ay);
            Fly(Ax, Ay);
            Spot(10.0);
            Fly(Ax + 2.0, Ay + 2.0);
            Scribble(To_String(Gp.Id), 0.5, Left, Bottom);
         end if;
         C := Next(C);
      end loop;

      -- Save this as the background for animation.
      Save;
   end Initialize;

   procedure Update(State : in Simulation_State_Type) is

      -- Print the simulation clock value in the lower left corner.
      procedure Update_Simulation_Clock is
      begin
         Fly(2.0, 2.0);
         Scribble(Image(State.Time), 1.0, Left, Bottom);
      end Update_Simulation_Clock;

      -- Show a small graphic for each friend enroute.
      procedure Update_Friends_Enroute is
         use Friend_Enroute_Lists;
         C : Cursor;
         F : Friend_Enroute_Type;
         X, Y : Float;
      begin
         C := First(State.Friends_Enroute);
         while C /= No_Element loop
            World_To_Viewport(Element(C).Location, X, Y);
            Fly(X, Y);
            Spot("friend.png");
            C := Next(C);
         end loop;
      end Update_Friends_Enroute;

      -- Show a small graphic for each hazard.
      procedure Update_Emplaced_Hazards is
         use Emplaced_Hazard_Lists;
         C : Cursor;
         X, Y : Float;
      begin
         C := First(State.Hazards);
         while C /= No_Element loop
            World_To_Viewport(Element(C).Location, X, Y);
            Fly(X, Y);
            Spot("ied.png");
            C := Next(C);
         end loop;
      end Update_Emplaced_Hazards;

   begin
      -- Initiize, retore the background computed earlier, then draw
      -- the graphics that change in every frame on top of the background.
      Initialize(State);
      if Enabled_P then
         Restore;
         Update_Simulation_Clock;
         Update_Friends_Enroute;
         Update_Emplaced_Hazards;
         Pause(0.0); -- Flushes all graphics out to the screen.
      end if;
   end Update;

   -- Close turtlegraphics.
   procedure Shut_Down is
   begin
      if Initialized_P then
         End_World;
      end if;
   end Shut_Down;

end Animation;
