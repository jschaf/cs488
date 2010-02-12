-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Containers.Doubly_Linked_Lists;

package Parser is
   
   type Node_T is abstract tagged null record;
   type Node_A is access Node_T;
   
   subtype String_Handle_Type is Natural;
   
   function Node_A_Equals (Left, Right : in Node_A) return Boolean;
   
   package Node_A_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Node_A,
      "=" => Node_A_Equals);

   
   type Model_NT;
   type Description_Section_NT;
   type Named_Description_Head_NT;
   type Named_Description_Tail_NT;
   type Parameter_List_NT;
   type Parameter_NT;
   type Type_ID_NT;
   type Description_NT;
   type Segment_Description_NT;
   type Route_Description_NT;
   type Segment_List_NT;
   type Segment_ID_NT;
   type Friend_Description_NT;
   type Trip_Description_NT;
   type Threat_Description_NT;
   type Schedule_Description_NT;
   type Sensor_Description_NT;
   type With_Attributes_NT;
   type Attribute_List_NT;
   type Attribute_Pair_NT;
   type ID_List_NT;
   type Instance_Section_NT;
   type Instance_NT;
   type Expr_List_NT;
   type Expr_Attribute_Name_NT;
   type Expr_NT;
   type Term_NT;
   type Signed_Factor_NT;
   type Factor_NT;
   type Random_Var_NT;
   type Add_Op_NT;
   type Mul_Op_NT;

   type Model_NT is new Node_T with record
      Description_Section : access Description_Section_NT;
      Instance_Section : access Instance_Section_NT;
   end record;

   type Description_Section_NT is new Node_T with record
      Named_Description_List : Node_A_Lists.List;
   end record;

   type Named_Description_Head_NT is new Node_T with record
      Identifier : String_Handle_Type;
   end record;

   type Named_Description_Tail_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Parameter_List_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Parameter_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Type_ID_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Segment_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Route_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Segment_List_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Segment_ID_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Friend_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Trip_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Threat_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Schedule_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Sensor_Description_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type With_Attributes_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Attribute_List_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Attribute_Pair_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type ID_List_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Instance_Section_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Instance_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Expr_List_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Expr_Attribute_Name_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Expr_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Term_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Signed_Factor_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Factor_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Random_Var_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Add_Op_NT is new Node_T with record
      Blah : Node_A;
   end record;

   type Mul_Op_NT is new Node_T with record
      Blah : Node_A;
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
