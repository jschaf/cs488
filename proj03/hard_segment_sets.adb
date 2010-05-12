with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

package body Hard_Segment_Sets is

   use Segment_Vectors;

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

end Hard_Segment_Sets;
