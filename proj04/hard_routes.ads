-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Containers.Vectors;
with Ada.Containers, Ada.Numerics.Elementary_Functions;
use  Ada.Containers, Ada.Numerics.Elementary_Functions;

with Common;
use  Common;

-- Limited with breaks a cyclic dependency.
limited with Simulator;

package Hard_Routes is

   -- A point on a hard route.  The trafficability applies from this point
   -- to the next one in the hard route vector below.
   type Route_Point_Type is
      record
         Point : Point_2d_Type;
         Trafficability : Real := 1.0;
         Vulnerability : Real := 1.0;
      end record;

   -- Set up vectors of route points, which we will call hard routes.
   -- This is to disginguish them from the route entities in abstract
   -- syntax trees.  A hard route of N points has N-1 segments.  Therefore
   -- the trafficability field of the last point is meaningless.
   package Hard_Route_Vectors is
     new Ada.Containers.Vectors(Positive, Route_Point_Type, "=");
   subtype Hard_Route_Type is Hard_Route_Vectors.Vector;
   type Hard_Route_Ptr_Type is access all Hard_Route_Type;

   -- Return number of segments in a hard routes (number of route points - 1)
   function N_Segments(Route : in Hard_Route_Type) return Natural;

   -- Return the length of the Index'th segment in the route.
   function Segment_Length(Route : in Hard_Route_Type;
                           Index : in Positive) return Real;

   -- A valid Position along a Route is a number in the range 0.0 through
   -- Total_Length(Route) equal to distance travelled since the start,
   -- Element(Route, 1).

   -- Return information about the segment that contains the given Position
   -- along the given Route.  If Position is larger than the total length
   -- of the route, it is set equal to that value.
   procedure Get_Segment(Route : in Hard_Route_Type;
                         Position : in out Real;
                         N_Segs, Index : out Positive;
                         Seg_Start, Seg_Length : out Real;
                         Point : out Point_2d_Type);

   -- Representation of an entity that can stop an object advancing along
   -- a route.  See Advance() below.
   type Stopper_Type is
      record
         Id : Natural := 0;
         Location : Point_2d_Type;
      end record;

   -- A list of stoppers of arbitrary length.
   type Stopper_List_Type is array(Positive range <>) of Stopper_Type;

   -- Assuming an object is at given Position along Route travelling at
   -- Native_Speed adjusted for route trafficability and that a given
   -- Elapsed_Time has passed, update Position to a new value showing
   -- progress along the route.  The new geometric location is also
   -- returned in Point, and the flag Is_Complete is set true iff the end
   -- of the route has been reached during the time interval.  Any distance
   -- that would have been covered past the final route point is ignored.
   --
   -- If the object passes within Stopper_Radius of a Stopper while advancing
   -- at any time, then Stopper_Id is set to the Id recorded in that stopper,
   -- otherwise to zero.
   procedure Advance(Route : in Hard_Route_Type;
                     Native_Speed : in Real;
                     Travel_Time : in Real;
                     Stopper_Radius : in Real;
                     Stoppers : in Stopper_List_Type;
                     Segment_Index : out Positive;
                     Position : in out Real;
                     Point : out Point_2d_Type;
                     Is_Complete : out Boolean;
                     Stopper_Id : out Natural);

   procedure Put(Point : in Route_Point_Type);
   procedure Put(Route : in Hard_Route_Type);

end Hard_Routes;
