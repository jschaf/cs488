-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

package Parser is
   
   type Node_T is abstract tagged null record;
   type Node_A is access Node_T;

   -- Parse the input string as a TL command.  Raise a Syntax_Error exception
   -- if parsing fails.  Scanner may also raise Bad_Character if it sees
   -- a character it does not know.  Return a syntax tree for the input
   -- string in Rtn_Tree.  If Debug_Mode is true, print debugging information
   -- about the progress of the parsing.
   procedure Parse (S : in String;
                    Rtn_Tree : out Node_A;
                    Debug_Mode : in Boolean := False);

   Syntax_Error : exception;
   
end Parser;
