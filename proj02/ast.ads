-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Containers.Hashed_Maps, Ada.Unchecked_Deallocation;

with Common, String_Table;
use  Common, String_Table;

package AST is

   type Node_Type;
   type Node_Ptr_Type is access all Node_Type'Class;

   -- A symbol table entry is just an id and corresponding value, which
   -- is always a syntax tree.
   type Symbol_Table_Entry_Type is record
      Id : String_Handle_Type;
      Value : Node_Ptr_Type;
   end record;

   -- Set up a map from string handles to symbol table entries.
   -- This is the heart of the symbol table.  Note we use the
   -- hash function provided in String_Table.
   package Symbol_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type => String_Handle_Type,
                                     Element_Type => Symbol_Table_Entry_Type,
                                     Hash => Hash,
                                     Equivalent_Keys => "=",
                                     "=" => "=");

   type Symbol_Table_Type;

   type Symbol_Table_Ptr_Type is
     access Symbol_Table_Type;

   type Symbol_Table_Type is record
      Prev : Symbol_Table_Ptr_Type;
      Map : Symbol_Maps.Map;
   end record;

   -- Since the word "type" is an Ada keyword, we'll use the word "Tag"
   -- instead throughout the MIEDS code.
   type Tag_Type is (Null_Tag,
                     Error_Tag,
                     Void_Tag,
                     Geopoint_Tag,
                     Segment_Tag,
                     Route_Tag,
                     Friend_Tag,
                     Sensor_Tag,
                     Schedule_Tag,
                     Trip_Tag,
                     Threat_Tag,
                     Instance_Tag,
                     Model_Tag,
                     Number_Tag,
                     Constant_Number_Tag);

   -- The parent of all syntax tree node types.  By making this
   -- abstract, we prevent anyone from creating a Node_Type with "new".
   -- This is good becase Node_Type has no semantic value (meaning).
   type Node_Type is abstract tagged record
      Tag : Tag_Type := Null_Tag;
      N_Visits : Natural := 0;
   end record;

   ---------------------------------------------------------------------
   -- Primitive operations on Nodes:
   ---------------------------------------------------------------------
   -- Print tree nodes in XML for development purposes.
   procedure Put(Tree : access Node_Type) is abstract;

   -- Perform the type checking pass over the syntax tree.  Types are
   -- stored in the "Tag" field of each node.  The special Error_Tag
   -- is assigned if no valid type is possible.  The given symbol table
   -- serves as a type environment.  It maps identifiers to syntax trees
   -- that already have types assigned to their Tag fields.
   procedure Type_Check(Tree : access Node_Type;
			Symbol_Table : in Symbol_Table_Ptr_Type) is abstract;

   -- Perform the "expand" pass over the syntax tree.  This eliminates all
   -- id references to definitions that require parameters.  The
   -- input tree is not changed.  A completely fresh tree is returned.  It's
   -- called the expanded tree.  The given symbol table holds name-to-definition
   -- and formal parameter name-to-actual parameter value mappings.
   procedure Expand(Tree : access Node_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is abstract;

   -- Peform the "finish" pass over the syntax tree.  This eliminates the
   -- remaining id references, which can create cycles, so the tree is
   -- a graph.  Don't try to "Put" the result.  You'll wait a long time for
   -- it to finish printing!  This pass also evaluates the expressions for
   -- the random variable parameters and stores them as "hard" values by
   -- allocating the respective types from package random_variables.  Routes
   -- are also stored in an efficent "hard" form so the simulator can advance
   -- friends over routes without repeated searches through the syntax graph.
   procedure Finish(Tree : access Node_Type);

   -- Find a tree's numerical value.  This is only useful for trees of type
   -- number or constant_number.  If it's a number (i.e. contains a
   -- random variable somewhere), successive calls on the same tree could
   -- (and probably will) return different values.
   procedure Find_Value_Of(Tree : access Node_Type;
                           Value : out Real);

   ---------------------------------------------------------------------
   -- End of primitive operations on Nodes
   ---------------------------------------------------------------------

   -- Raised by Expand or Finish passes if a type error is discovered.
   Type_Error : exception;

   ---------------------------------------------------------------------

   -- Define a null node type.  We use this rather than null pointers
   -- to represent empty trees because it eliminates a bazzilion "if"
   -- statements to check for recursion base cases.  Instead we just
   -- define do-nothing primitive operations.
   type Null_Type is new Node_Type with null record;

   procedure Put(Tree : access Null_Type);
   procedure Type_Check(Tree : access Null_Type;
			Symbol_Table : in Symbol_Table_Ptr_Type) is null;
   procedure Expand(Tree : access Null_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Null_Type) is null;

   Null_Node_Ptr : constant Node_Ptr_Type := new Null_Type;

end AST;
