-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Common, AST, AST.Trees;
use  Common, AST, AST.Trees;

package Simulator is

   -- Placeholder for later...
   type Simulation_State_Type is null record;

   procedure Initialize(State : out Simulation_State_Type;
                        Model : in Node_Ptr_Type);

   -- Run the simulation.
   procedure Run(State : in out Simulation_State_Type;
                 Stop_Time : in Real);

end Simulator;
