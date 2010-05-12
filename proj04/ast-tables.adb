-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Strings.Fixed;
use  Ada.Text_IO, Ada.Strings.Fixed;

package body AST.Tables is

   use Symbol_Maps;

   ------------
   -- Insert --
   ------------

   procedure Insert (Symbol_Table : in Symbol_Table_Ptr_Type;
                     Id           : in String_Handle_Type;
                     Value        : in Node_Ptr_Type) is
   begin
      Symbol_Maps.Insert(Symbol_Table.Map,
                         Id,
                         Symbol_Table_Entry_Type'(Id => Id, Value => Value));
   exception
      when Constraint_Error =>
         raise Redefinition with To_String(Id);
   end Insert;

   -------------
   -- Look_Up --
   -------------

   procedure Look_Up (Symbol_Table : in Symbol_Table_Ptr_Type;
                      Id           : in String_Handle_Type;
                      Value        : out Node_Ptr_Type) is
      C : Cursor;
      T : Symbol_Table_Ptr_Type := Symbol_Table;
   begin
      while T /= null loop
         C := Find(T.Map, Id);
         if C /= No_Element then
            Value := Element(C).Value;
            return;
         end if;
         T := T.Prev;
      end loop;
      raise Undefined with To_String(Id);
   end Look_Up;

   -- For debugging, print symbol table contents.
   procedure Mark(Msg : in String;
                  Mark_Char : in Character) is
      Marker : String := 40 * Mark_Char;
      Start : constant Positive := 5;
   begin
      Marker(Start .. Start + Msg'Length + 1) := " " & Msg & " ";
      Put_Line(Marker);
   end Mark;

   procedure Put (Symbol_Table : in Symbol_Table_Ptr_Type) is
      T : Symbol_Table_Ptr_Type := Symbol_Table;
      N : Natural := 0;
      C : Cursor;
   begin
      Mark("Symbol Table Contents", '*');
      while T /= null loop
         Mark("Scope" & Natural'Image(N), '"');
         C := First(T.Map);
         while C /= No_Element loop
            Mark(To_String(Key(C)), '-');
            Put(Element(C).Value);
            C := Next(C);
         end loop;
         T := T.Prev;
         N := N + 1;
      end loop;
   end Put;

end AST.Tables;
