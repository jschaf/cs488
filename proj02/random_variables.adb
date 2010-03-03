-- Simple random variables with various distributions.
-- COL Gene Ressler, 31 December 2007.

with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

package body Random_Variables is

   Uniform_Initiator     : Integer := Integer'First;
   Exponential_Initiator : Integer := Integer'First;
   Normal_Initiator      : Integer := Integer'First;

   -----------------
   ---- Uniform ----
   -----------------

   procedure Get_Random
     (Gen : in out Uniform_Random_Variable_Type;
      X : out Real)
   is
   begin
      X := Real(Gen.Min + (Gen.Max - Gen.Min) * Random (Gen.Gen));
   end Get_Random;

   procedure Reset (Gen : in out Uniform_Random_Variable_Type) is
   begin
      Reset(Gen.Gen, Uniform_Initiator);
      Uniform_Initiator := Uniform_Initiator + 1;
   end Reset;

   procedure Set
     (Gen : in out Uniform_Random_Variable_Type;
      Min, Max : in Real)
   is
   begin
      Gen.Min := Float(Min);
      Gen.Max := Float(Max);
      Reset(Gen);
   end Set;

   ---------------------
   ---- Exponential ----
   ---------------------

   procedure Get_Random
     (Gen : in out Exponential_Random_Variable_Type;
      X : out Real)
   is
   begin
      X := Real(-Gen.Lambda * Log(Random(Gen.Gen) + Float'Model_Small));
   end Get_Random;

   procedure Reset (Gen : in out Exponential_Random_Variable_Type) is
   begin
      Reset(Gen.Gen, Exponential_Initiator);
      Exponential_Initiator := Exponential_Initiator + 1;
   end Reset;

   procedure Set
     (Gen : in out Exponential_Random_Variable_Type;
      Beta : in Real)
   is
   begin
      Gen.Lambda := Float(Beta);
      Reset(Gen);
   end Set;

   ----------------
   ---- Normal ----
   ----------------

   procedure Get_Random
     (Gen : in out Normal_Random_Variable_Type;
      X : out Real)
   is
      V1, V2, W, Y : Float;
   begin
      if Gen.X2_Set then
         Gen.X2_Set := False;
         X := Real(Gen.X2);
      end if;
      loop
         V1 := 2.0 * Random(Gen.Gen1) - 1.0;
         V2 := 2.0 * Random(Gen.Gen2) - 1.0;
         W := V1 ** 2 + V2 ** 2 + Float'Model_Small;
         exit when W <= 1.0;
      end loop;
      Y := Sqrt((-2.0 * Log(W)) / W);
      Gen.X2 := Gen.Mu + Gen.Sigma * V2 * Y;
      Gen.X2_Set := True;
      X := Real(Gen.Mu + Gen.Sigma * V1 * Y);
   end Get_Random;

   procedure Reset (Gen : in out Normal_Random_Variable_Type) is
   begin
      Reset(Gen.Gen1, Normal_Initiator);
      Reset(Gen.Gen2, Normal_Initiator + 1);
      Normal_Initiator := Normal_Initiator + 2;
   end Reset;

   procedure Set
     (Gen : in out Normal_Random_Variable_Type;
      Mu, Sigma : in Real)
   is
   begin
      Gen.Mu := Float(Mu);
      Gen.Sigma := Float(Sigma);
      Reset(Gen);
   end Set;

end Random_Variables;
