-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Common, Ast.Trees, Simulator;
use  Common, Ast.Trees, Simulator;

package Animation is

   procedure Enable(Val : in Boolean := True);
   procedure Initialize(State : in Simulation_State_Type);
   procedure Update(State : in Simulation_State_Type);
   procedure Shut_Down;

end Animation;
