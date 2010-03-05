-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

package body Hard_Segment_Sets is

   use Segment_Vectors;

   -- Return the total length of all the segments in a set.
   function Length(Segment_Set : in Segment_Set_Type) return Real is
      L : Real := 0.0;
      C : Cursor := First(Segment_Set);
   begin
      while C /= No_Element loop
         L := L + Distance(Element(C).A, Element(C).B);
         C := Next(C);
      end loop;
      return L;
   end Length;

   Gen : Generator;

   -- Get a random point that lies on a set of segments.
   procedure Get_Random_Point(Segment_Set : in Segment_Set_Type;
                              Point : out Point_2d_Type) is
      L, L0, D : Real := 0.0;
      C : Cursor := First(Segment_Set);
   begin
      L0 := Length(Segment_Set) * Real(Random(Gen));
      while C /= No_Element loop
         D := Distance(Element(C).A, Element(C).B);
         L := L + D;
         if L >= L0 and D > 0.0 then
            Point := Lerp(Element(C).A, Element(C).B,
                          (L - L0) / D);
            return;
         end if;
         C := Next(C);
      end loop;
      Point := Last_Element(Segment_Set).B;
   end Get_Random_Point;

begin
   -- Cause the same random numbers to be generated for each run.
   Reset(Gen, 42 * 42);
end Hard_Segment_Sets;
