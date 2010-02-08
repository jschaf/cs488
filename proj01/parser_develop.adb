-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Scanner, Parser;
use  Scanner, Parser;

procedure Parser_Develop is
   S    : Buffer_A := Read_To_String("example.sim");
   Tree : Node_A;
begin
   -- Run the parser on it.
   Parse(S          => S.all,
         Rtn_Tree   => Tree,
         Debug_Mode => True);
end Parser_Develop;
