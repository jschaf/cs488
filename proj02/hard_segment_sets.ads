-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Common;
use  Common;

with Ada.Containers.Vectors;

package Hard_Segment_Sets is

   -- A point on a hard route.  The trafficability applies from this point
   -- to the next one in the hard route vector below.
   type Segment_Set_Element_Type is
      record
         A, B : Point_2d_Type;
      end record;

   package Segment_Vectors is
     new Ada.Containers.Vectors(Positive, Segment_Set_Element_Type, "=");
   subtype Segment_Set_Type is Segment_Vectors.Vector;

   procedure Get_Random_Point(Segment_Set : in Segment_Set_Type;
                              Point : out Point_2d_Type);

end Hard_Segment_Sets;
