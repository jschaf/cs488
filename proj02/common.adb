with Ada.Long_Float_Text_IO, Ada.Strings, Ada.Strings.Fixed;
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Long_Float_Text_IO, Ada.Strings, Ada.Strings.Fixed;
use  Ada.Numerics.Long_Elementary_Functions;

package body Common is

   function Image (X : Real) return String is
      Buf : String(1..32);
   begin
      Put(Buf, X, 5, 0);
      return Trim(Buf, Left);
   end Image;

   function Image (Point : Point_2d_Type) return String is
   begin
      return "(" & Image(Point.X) & ", " & Image(Point.Y) & ")";
   end Image;

   -- Define common some operations on tuples, vectors, and points,
   function Magnitude_Squared(Right : in Tuple_2d_Type) return Real is
   begin
      return Right.X ** 2 + Right.Y ** 2;
   end Magnitude_Squared;

   function "abs" (Right : in Tuple_2d_Type) return Real is
   begin
      return Sqrt(Magnitude_Squared(Right));
   end "abs";

   function "+" (Left, Right : in Vector_2d_Type) return Vector_2d_Type is
   begin
      return (X => Left.X + Right.X, Y => Left.Y + Right.Y);
   end "+";

   function "+" (Left : in Point_2d_Type;
                 Right : in Vector_2d_Type) return Point_2d_Type is
   begin
      return (X => Left.X + Right.X, Y => Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : in Vector_2d_Type) return Vector_2d_Type is
   begin
      return (X => Left.X - Right.X, Y => Left.Y - Right.Y);
   end "-";

   function "-" (Left : in Point_2d_Type;
                 Right : in Vector_2d_Type) return Point_2d_Type is
   begin
      return (X => Left.X - Right.X, Y => Left.Y - Right.Y);
   end "-";

   function "-" (Left, Right : in Point_2d_Type ) return Vector_2d_Type is
   begin
      return (X => Left.X - Right.X, Y => Left.Y - Right.Y);
   end "-";

   function "*" (Left : in Real;
                 Right : in Vector_2d_Type) return Vector_2d_Type is
   begin
      return (X => Left * Right.X, Y => Left * Right.Y);
   end "*";

   function "/" (Left : in Vector_2d_Type;
                 Right : in Real) return Vector_2d_Type is
   begin
      return (X => Left.X / Right, Y => Left.Y / Right);
   end "/";

   function Unit(Right : in Vector_2d_Type) return Vector_2d_Type is
   begin
      return Right / abs Right;
   end Unit;

   function Perp(Right : in Vector_2d_Type) return Vector_2d_Type is
   begin
      return (X => -Right.Y, Y => Right.X);
   end Perp;

   function Dot (Left, Right : in Vector_2d_Type) return Real is
   begin
      return Left.X * Right.X + Left.Y * Right.Y;
   end Dot;

   function Cross(Left, Right : in Vector_2d_Type) return Real is
   begin
      return Left.X * Right.Y - Right.X * Left.Y;
   end Cross;

   -- Return the distance from point A to B.
   function Distance(A, B : in Point_2d_Type) return Real is
   begin
      return Sqrt((A.X - B.X) ** 2 + (A.Y - B.Y) ** 2);
   end Distance;

   -- Linearly interpolate points A and B using parameter T.
   -- Lerp(A, B, 0) = A  and  Lerp(A, B, 1) = B.
   function Lerp(A, B : in Point_2d_Type;
                 T : in Real) return Point_2d_Type is
   begin
      return (X => A.X + T * (B.X - A.X),
              Y => A.Y + T * (B.Y - A.Y));
   end Lerp;

end Common;
