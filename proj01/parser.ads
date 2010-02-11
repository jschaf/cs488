-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

package Parser is
   
   type Node_T is abstract tagged null record;
   
   -- This is a placeholder for future syntax tree node pointer type.
   type Node_A is access Node_T;
   
   type Model_Node_T is new Node_T with record
      Description_Section : Node_A;
      Instance_Section : Node_A;
   end record;
   
   type Description_Section_Node_T is new Node_T with record
      Named_Description_List : Integer;
   end record;
     
   

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
