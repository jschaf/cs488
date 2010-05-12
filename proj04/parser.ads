-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with AST, AST.Trees;
use  AST, AST.Trees;

package Parser is

   -- Parse the input string as a MIEDS program. Raise a Syntax_Error exception
   -- if parsing fails. Scanner may also raise Bad_Character if it sees
   -- a character it does not know. Return a syntax tree for the input
   -- string in Rtn_Tree. If Debug_Mode is true, print debugging information
   -- about the progress of the parsing.
   procedure Parse (S : in String;
                    Rtn_Tree : out Node_Ptr_Type;
                    Debug_Mode : in Boolean := False);

   Syntax_Error : exception;

end Parser;
