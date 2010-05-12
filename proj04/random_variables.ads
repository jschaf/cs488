-- Simple random variables with various distributions.
-- COL Gene Ressler, 31 December 2007.

with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

with Common;
use  Common;

package Random_Variables is

   -- Base random generator.
   type Random_Variable_Type is abstract tagged limited null record;
   type Random_Variable_Ptr_Type is
     access all Random_Variable_Type'Class;

   -- Return a random number with distribution given by derived type.
   procedure Get_Random(Gen : in out Random_Variable_Type;
                        X : out Real) is abstract;

   -- Reset "uniqifies" the stream of the Variable to make it different
   -- from all others (up to a billion or so, anyway).
   procedure Reset(Gen : in out Random_Variable_Type) is abstract;

   -- Uniformly distributed random numbers.  Default is [0..1], but
   -- Set allows this to be changed.  Set calls Reset to uniquify the stream.
   type Uniform_Random_Variable_Type is
     new Random_Variable_Type with private;
   procedure Get_Random(Gen : in out Uniform_Random_Variable_Type;
                        X : out Real);
   procedure Reset(Gen : in out Uniform_Random_Variable_Type);
   procedure Set(Gen : in out Uniform_Random_Variable_Type;
                 Min, Max : in Real);

   -- Exponentially distributed random numbers.  Default is beta=1, but
   -- Set allows this to be changed.  Set calls Reset to uniquify the stream.
   type Exponential_Random_Variable_Type is
     new Random_Variable_Type with private;
   procedure Get_Random(Gen : in out Exponential_Random_Variable_Type;
                        X : out Real);
   procedure Reset(Gen : in out Exponential_Random_Variable_Type);
   procedure Set(Gen : in out Exponential_Random_Variable_Type;
                 Beta : in Real);

   -- Uniformly distributed random numbers.  Default is [0..1], but
   -- Set allows this to be changed.  Set calls Reset to uniquify the stream.
   type Normal_Random_Variable_Type is
     new Random_Variable_Type with private;
   procedure Get_Random(Gen : in out Normal_Random_Variable_Type;
                        X : out Real);
   procedure Reset(Gen : in out Normal_Random_Variable_Type);
   procedure Set(Gen : in out Normal_Random_Variable_Type;
                 Mu, Sigma : in Real);

private

   type Uniform_Random_Variable_Type is
     new Random_Variable_Type with
      record
         Gen : Generator;
         Min : Float := 0.0;
         Max : Float := 1.0;
      end record;

   type Exponential_Random_Variable_Type is
     new Random_Variable_Type with
      record
         Gen : Generator;
         Lambda : Float := 1.0;
      end record;

   type Normal_Random_Variable_Type is
     new Random_Variable_Type with
      record
         Gen1, Gen2 : Generator;
         Mu : Float := 0.0;
         Sigma : Float := 1.0;
         X2 : Float := 0.0;
         X2_Set : Boolean := False;
      end record;

end Random_Variables;
