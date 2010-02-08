-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Scanner, Parser;
use  Scanner, Parser;

procedure Parser_Develop is
   S : Buffer_A;
   Tree : Node_A;
begin
   -- Read the example file to a string.
   Read_To_String("example.sim", S);

   -- Run the parser on it.
   Parse(S          => S.all,
         Rtn_Tree   => Tree,
         Debug_Mode => True);
end Parser_Develop;
