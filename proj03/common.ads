-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

package Common is

   -- This allows for a simple change to Long_Float if needed.
   subtype Real is Long_Float;

   function Image(X : Real) return String;

   -- Simple point-vector operations.

   -- A 2-tuple is jsut an x-y pair.
   type Tuple_2d_Type is
      record
         X, Y : Real := 0.0;
      end record;

   function Magnitude_Squared(Right : in Tuple_2d_Type) return Real;
   function "abs" (Right : in Tuple_2d_Type) return Real;

   -- Vectors and points are particular kinds of tuples.
   type Vector_2d_Type is new Tuple_2d_Type;
   type Point_2d_Type is new Tuple_2d_Type;

   -- Define common some operations on tuples, vectors, and points,
   function "+" (Left, Right : in Vector_2d_Type) return Vector_2d_Type;
   function "+" (Left : in Point_2d_Type;
                 Right : in Vector_2d_Type) return Point_2d_Type;
   function "-" (Left, Right : in Vector_2d_Type) return Vector_2d_Type;
   function "-" (Left : in Point_2d_Type;
                 Right : in Vector_2d_Type) return Point_2d_Type;
   function "-" (Left, Right : in Point_2d_Type ) return Vector_2d_Type;
   function "*" (Left : in Real;
                 Right : in Vector_2d_Type) return Vector_2d_Type;
   function "/" (Left : in Vector_2d_Type;
                 Right : in Real) return Vector_2d_Type;
   function Unit(Right : in Vector_2d_Type) return Vector_2d_Type;
   function Perp(Right : in Vector_2d_Type) return Vector_2d_Type;
   function Dot (Left, Right : in Vector_2d_Type) return Real;
   function Cross(Left, Right : in Vector_2d_Type) return Real;

   function Image(Point : Point_2d_Type) return String;

   -- Return the distance from point A to B.
   function Distance(A, B : in Point_2d_Type) return Real;

   -- Linearly interpolate points A and B using parameter T.
   -- Lerp(A, B, 0.0) = A  and  Lerp(A, B, 1.0) = B.
   function Lerp(A, B : in Point_2d_Type;
                 T : in Real) return Point_2d_Type;

end Common;
