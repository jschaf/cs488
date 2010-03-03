-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Tags, Ada.Numerics.Float_Random;
use  Ada.Text_IO, Ada.Tags, Ada.Numerics.Float_Random;

with Ada.Unchecked_Deallocation;

with Hard_Routes, Hard_Segment_Sets;
use  Hard_Routes, Hard_Segment_Sets;

package body Simulator is

   -- Initailize the simluation state using the given model.
   procedure Initialize(State : out Simulation_State_Type;
			Model : in Node_Ptr_Type) is
   begin
      null;
   end Initialize;

   ---------------------------------------------------------------------
   -- Simulation loop
   ---------------------------------------------------------------------

   -- Run the simulation event loop up to given stop time.
   procedure Run(State : in out Simulation_State_Type;
                 Stop_Time : in Real) is
   begin
      null;
   end Run;

end Simulator;
