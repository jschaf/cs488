-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Tags;
use  Ada.Text_IO, Ada.Tags;

package body AST is

   procedure Find_Value_Of(Tree : access Node_Type;
                           Value : out Real) is
   begin
      Value := 0.0;
      if Tree = null then
         raise Constraint_Error;
      else
         raise Constraint_Error
           with "attempt to find value of a " &
             Expanded_Name(Node_Ptr_Type(Tree).all'Tag);
      end if;
   end Find_Value_Of;

   procedure Finish(Tree : access Node_Type) is
   begin
      if Tree = null then
         raise Constraint_Error;
      else
         raise Constraint_Error
           with "attempt to finish a " &
             Expanded_Name(Node_Ptr_Type(Tree).all'Tag);
      end if;
   end Finish;

   procedure Put(Tree : access Null_Type) is
   begin
      Put_Line("<null />");
   end Put;

   procedure Expand(Tree : access Null_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
   begin
      Expanded_Tree := Node_Ptr_Type(Tree);
   end Expand;

end AST;
