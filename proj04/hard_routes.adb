-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Containers.Vectors;
with Ada.Text_IO, Ada.Long_Float_Text_IO, Ada.Containers;
use  Ada.Text_IO, Ada.Long_Float_Text_IO, Ada.Containers;

package body Hard_Routes is

   use Hard_Route_Vectors;

   function N_Segments(Route : in Hard_Route_Type) return Natural is
   begin
      return Natural(Length(Route) - 1);
   end N_Segments;

   -- Return the length of the Index'th segment in the route.
   function Segment_Length(Route : in Hard_Route_Type;
                           Index : in Positive) return Real is
   begin
      return Distance(Element(Route, Index).Point,
                      Element(Route, Index + 1).Point);
   end Segment_Length;

   procedure Get_Segment(Route : in Hard_Route_Type;
                         Position : in out Real;
                         N_Segs, Index : out Positive;
                         Seg_Start, Seg_Length : out Real;
                         Point : out Point_2d_Type) is
   begin
      N_Segs := N_Segments(Route);
      Seg_Start := 0.0;
      Index := 1;
      loop
         Seg_Length := Segment_Length(Route, Index);
         if Seg_Start + Seg_Length > Position then
            Position := Real'Max(0.0, Position);
            Point :=  Lerp(Element(Route, Index).Point,
                           Element(Route, Index + 1).Point,
                           (Position - Seg_Start) / Seg_Length);
            exit;
         end if;
         if Index = N_Segs then
            Position := Seg_Start + Seg_Length;
            Point := Last_Element(Route).Point;
            exit;
         end if;
         Index := Index + 1;
         Seg_Start := Seg_Start + Seg_Length;
      end loop;
   end Get_Segment;

   procedure Advance(Route : in Hard_Route_Type;
                     Native_Speed : in Real;
                     Travel_Time : in Real;
                     Stopper_Radius : in Real;
                     Stoppers : in Stopper_List_Type;
                     Segment_Index : out Positive;
                     Position : in out Real;
                     Point : out Point_2d_Type;
                     Is_Complete : out Boolean;
                     Stopper_Id : out Natural) is

      function Speed(I : in Positive) return Real is
      begin
         return Native_Speed * Element(Route, I).Trafficability;
      end Speed;

      procedure Check_Stoppers(P, Q : in Point_2d_Type;
                               Stopper_Id : out Natural;
                               S : in out Real;
                               Point : in out Point_2d_Type) is
         B : constant Vector_2d_Type := Q - P;
         One_Over_B_Dot_B : constant Real := 1.0 / Dot(B, B);
         T_Min : Real := Real'Adjacent(1.0, 2.0);
         T, D : Real;
         A : Vector_2d_Type;
         R : Point_2d_Type;
      begin
         Stopper_Id := 0;
         for I in Stoppers'Range loop
            A := Stoppers(I).Location - P;
            if abs A <= Stopper_Radius then
               Stopper_Id := Stoppers(I).Id;
               T_Min := 0.0;
            elsif 1.0 < T_Min and then
                  Distance(Stoppers(I).Location, Q) <= Stopper_Radius then
               Stopper_Id := Stoppers(I).Id;
               T_Min := 1.0;
            else
               T := Dot(A, B) * One_Over_B_Dot_B;
               if 0.0 <= T and T < T_Min then
                  R := P + T * B;
                  D := Distance(Stoppers(I).Location,  R);
                  if D <= Stopper_Radius then
                     Stopper_Id := Stoppers(I).Id;
                     T_Min := T;
                  end if;
               end if;
            end if;
         end loop;
      end Check_Stoppers;

      N_Segs : Positive;            -- Number of segments in route.
      Start_Point : Point_2d_Type;  -- Start point of current (sub)segment.
      Seg_Start,                    -- Position of segment start.
      Seg_Length,                   -- Linear length of current segment.
      T0, T1,                       -- Time to reach start/end of current seg.
      Distance_Traveled : Real;     -- Distance after last way point.
   begin
      -- Get segment of initial position.
      Get_Segment(Route, Position,
                  N_Segs, Segment_Index, Seg_Start, Seg_Length, Start_Point);
      -- Set times needed to reach start and end of current segment.
      T0 := 0.0;
      T1 := (Seg_Start + Seg_Length - Position) / Speed(Segment_Index);
      -- Loop while we can reach end of segment in alotted time.
      while T1 <= Travel_Time loop

         -- Check for first stopper encountered while traversing this segment.
         Check_Stoppers(P           => Start_Point,
                        Q           => Element(Route, Segment_Index + 1).Point,
                        Stopper_Id  => Stopper_Id,
                        S           => Position,
                        Point       => Point);
         if Stopper_Id > 0 then
            Is_Complete := False;
            return;
         end if;

         -- If no more segments left, we have completed the route.
         if Segment_Index = N_Segs then
            Position := Seg_Start + Seg_Length;
            Point := Last_Element(Route).Point;
            Is_Complete := True;
            Stopper_Id := 0;
            return;
         end if;

         -- Update all state variables to next segment.
         Segment_Index := Segment_Index + 1;
         Seg_Start := Seg_Start + Seg_Length;
         Position := Seg_Start;
         Seg_Length := Segment_Length(Route, Segment_Index);
         T0 := T1;
         T1 := T1 + Seg_Length / Speed(Segment_Index);
         Start_Point := Element(Route, Segment_Index).Point;
      end loop;

      -- Account for partial coverage of segment before time runs out.
      Distance_Traveled := (Travel_Time - T0) * Speed(Segment_Index);
      Position := Position + Distance_Traveled;
      Point := Lerp(Start_Point, Element(Route, Segment_Index + 1).Point,
                    Distance_Traveled / Seg_Length);
      Is_Complete := False;

      -- Check if partial traversal encountered a stopper.
      Check_Stoppers(P           => Start_Point,
                     Q           => Point,
                     Stopper_Id  => Stopper_Id,
                     S           => Position,
                     Point       => Point);
   end Advance;

   -- Print a plain opening tag.
   procedure Open(XML_Tag : in String) is
   begin
      Put("<" & XML_Tag & ">");
   end Open;

   -- Print a closing XML tag and a line break in the output.
   procedure Close(XML_Tag : in String) is
   begin
      Put_Line("</" & XML_Tag & ">");
   end Close;

   -- Put a real number.
   procedure Put_Real(Tag : in String; Val : in Real) is
   begin
      Open(Tag);
      Put(Image(Val));
      Close(Tag);
   end Put_Real;

   procedure Put(Point : in Route_Point_Type) is
   begin
      Open("route_point_type");
      Put_Real("x", Point.Point.X);
      Put_Real("y", Point.Point.Y);
      Put_Real("trafficability", Point.Trafficability);
      Put_Real("vulnerability", Point.Vulnerability);
      Close("route_point_type");
   end Put;

   procedure Put(Route : in Hard_Route_Type) is
      C : Cursor;
   begin
      Open("hard_route_type");
      C := First(Route);
      while C /= No_Element loop
         Put(Element(C));
         C := Next(C);
      end loop;
      Close("hard_route_type");
   end Put;

end Hard_Routes;
