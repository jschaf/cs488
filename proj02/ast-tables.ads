-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Unchecked_Deallocation;
with Common, String_Table;
use  Common, String_Table;

package AST.Tables is

   -- Insert an ID and corresponding value in a symbol table.  Raises
   -- a Redefinition exception if the ID is already in the table.
   procedure Insert
     (Symbol_Table : in Symbol_Table_Ptr_Type;
      Id           : in String_Handle_Type;
      Value        : in Node_Ptr_Type);

   Redefinition : exception;

   -- Look up the given Id in a symbol table and return its value.  Raises
   -- an Undefined exception if the ID is not in the table.
   procedure Look_Up
     (Symbol_Table : in Symbol_Table_Ptr_Type;
      Id           : in String_Handle_Type;
      Value        : out Node_Ptr_Type);

   Undefined : exception;

   -- Free storage used by a symbol table.
   procedure Free is
      new Ada.Unchecked_Deallocation(Symbol_Table_Type, Symbol_Table_Ptr_Type);

   -- For debugging, print symbol table contents.
   procedure Put(Symbol_Table : in Symbol_Table_Ptr_Type);

end AST.Tables;
