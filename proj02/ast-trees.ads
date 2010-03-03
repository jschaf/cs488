-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Containers.Vectors;

with Common, Random_Variables, String_Table, Hard_Routes, Hard_Segment_Sets;
use  Common, Random_Variables, String_Table, Hard_Routes, Hard_Segment_Sets;

package AST.Trees is

   -- Set up a vector type for use in storing lists of nodes of any length.
   package Node_Lists is
     new Ada.Containers.Vectors(Positive, Node_Ptr_Type, "=");
   subtype Node_List_Type is Node_Lists.Vector;

   ---------------------------------------------------------------------

   type Geopoint_Type is new Node_Type with record
      Id : String_Handle_Type := Null_String_Handle;
      X, Y : Node_Ptr_Type := Null_Node_Ptr;
   end record;

   type Geopoint_Ptr_Type is access all Geopoint_Type;
   procedure Put(Tree : access Geopoint_Type);
   procedure Type_Check(Tree : access Geopoint_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Geopoint_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Geopoint_Type);

   -- Convenience function to evaluate a Geopoint to a 2d point.
   function Value_Of(Geopoint : Geopoint_Ptr_Type) return Point_2d_Type;

   ---------------------------------------------------------------------

   type Segment_Type is new Node_Type with record
      A, B           : Node_Ptr_Type := Null_Node_Ptr;
      Trafficability : Node_Ptr_Type := Null_Node_Ptr;
      Vulnerability  : Node_Ptr_Type := Null_Node_Ptr;
   end record;

   type Segment_Ptr_Type is access all Segment_Type;
   procedure Put(Tree : access Segment_Type);
   procedure Type_Check(Tree : access Segment_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Segment_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Segment_Type);

   ---------------------------------------------------------------------

   type Route_Type is new Node_Type with record
      Segments : Node_List_Type;
      Hard_Route : aliased Hard_Route_Type;
   end record;

   type Route_Ptr_Type is access all Route_Type;
   procedure Put(Tree : access Route_Type);
   procedure Type_Check(Tree : access Route_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Route_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Route_Type);

   ---------------------------------------------------------------------

   type Friend_Type is new Node_Type with record
      Speed              : Node_Ptr_Type := Null_Node_Ptr;
      Vulnerability      : Node_Ptr_Type := Null_Node_Ptr;
      Effectiveness      : Node_Ptr_Type := Null_Node_Ptr;
      Sensor             : Node_Ptr_Type := Null_Node_Ptr;
   end record;

   type Friend_Ptr_Type is access all Friend_Type;
   procedure Put(Tree : access Friend_Type);
   procedure Type_Check(Tree : access Friend_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Friend_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Friend_Type);

   ---------------------------------------------------------------------

   type Sensor_Type is new Node_Type with record
      The_Range  : Node_Ptr_Type := Null_Node_Ptr;
      Responders : Node_List_Type;
      Effectiveness : Node_Ptr_TYpe := Null_Node_Ptr;
   end record;

   type Sensor_Ptr_Type is access all Sensor_Type;
   procedure Put(Tree : access Sensor_Type);
   procedure Type_Check(Tree : access Sensor_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Sensor_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Sensor_Type);

   ---------------------------------------------------------------------

   type Schedule_Type is new Node_Type with record
      Start    : Node_Ptr_Type := Null_Node_Ptr;
      Interval : Node_Ptr_Type := Null_Node_Ptr;
   end record;

   type Schedule_Ptr_Type is access all Schedule_Type;
   procedure Put(Tree : access Schedule_Type);
   procedure Type_Check(Tree : access Schedule_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Schedule_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Schedule_Type);

   ---------------------------------------------------------------------

   type Trip_Type is new Node_Type with record
      Friend   : Node_Ptr_Type := Null_Node_Ptr;
      Route    : Node_Ptr_Type := Null_Node_Ptr;
      Schedule : Node_Ptr_Type := Null_Node_Ptr;
   end record;

   type Trip_Ptr_Type is access all Trip_Type;
   procedure Put(Tree : access Trip_Type);
   procedure Type_Check(Tree : access Trip_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Trip_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Trip_Type);

   ---------------------------------------------------------------------

   type Threat_Type is new Node_Type with record
      Target_Segments    : Node_List_Type;
      Effectiveness      : Node_Ptr_Type := Null_Node_Ptr;
      Vulnerability      : Node_Ptr_Type := Null_Node_Ptr;
      Schedule           : Node_Ptr_Type := Null_Node_Ptr;
      Duration           : Node_Ptr_Type := Null_Node_Ptr;
      -- Filled in during Finish...
      Segment_Set : Segment_Set_Type;
   end record;

   type Threat_Ptr_Type is access all Threat_Type;
   procedure Put(Tree : access Threat_Type);
   procedure Type_Check(Tree : access Threat_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Threat_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Threat_Type);

   ---------------------------------------------------------------------

   type Add_Type is new Node_Type with
      record
         Lhs, Rhs : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Add_Ptr_Type is access all Add_Type;
   procedure Put(Tree : access Add_Type);
   procedure Type_Check(Tree : access Add_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Add_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Add_Type);
   procedure Find_Value_Of(Tree : access Add_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Sub_Type is new Node_Type with
      record
         Lhs, Rhs : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Sub_Ptr_Type is access all Sub_Type;
   procedure Put(Tree : access Sub_Type);
   procedure Type_Check(Tree : access Sub_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Sub_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Sub_Type);
   procedure Find_Value_Of(Tree : access Sub_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Mul_Type is new Node_Type with
      record
         Lhs, Rhs : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Mul_Ptr_Type is access all Mul_Type;
   procedure Put(Tree : access Mul_Type);
   procedure Type_Check(Tree : access Mul_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Mul_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Mul_Type);
   procedure Find_Value_Of(Tree : access Mul_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Div_Type is new Node_Type with
      record
         Lhs, Rhs : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Div_Ptr_Type is access all Div_Type;
   procedure Put(Tree : access Div_Type);
   procedure Type_Check(Tree : access Div_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Div_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Div_Type);
   procedure Find_Value_Of(Tree : access Div_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Neg_Type is new Node_Type with
      record
         X : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Neg_Ptr_Type is access all Neg_Type;
   procedure Put(Tree : access Neg_Type);
   procedure Type_Check(Tree : access Neg_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Neg_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Neg_Type);
   procedure Find_Value_Of(Tree : access Neg_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Uniform_Type is new Node_Type with
      record
         Gen : access Uniform_Random_Variable_Type := new Uniform_Random_Variable_Type;
         Min, Max : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Uniform_Ptr_Type is access all Uniform_Type;
   procedure Put(Tree : access Uniform_Type);
   procedure Type_Check(Tree : access Uniform_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Uniform_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Uniform_Type);
   procedure Find_Value_Of(Tree : access Uniform_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Exponential_Type is new Node_Type with
      record
         Gen : access Exponential_Random_Variable_Type := new Exponential_Random_Variable_Type;
         Beta : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Exponential_Ptr_Type is access all Exponential_Type;
   procedure Put(Tree : access Exponential_Type);
   procedure Type_Check(Tree : access Exponential_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Exponential_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Exponential_Type);
   procedure Find_Value_Of(Tree : access Exponential_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Normal_Type is new Node_Type with
      record
         Gen : access Normal_Random_Variable_Type := new Normal_random_variable_type;
         Mu, Sigma : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Normal_Ptr_Type is access all Normal_Type;
   procedure Put(Tree : access Normal_Type);
   procedure Type_Check(Tree : access Normal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Normal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Normal_Type);
   procedure Find_Value_Of(Tree : access Normal_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Literal_Type is new Node_Type with
      record
         Value : Real := 0.0;
      end record;

   type Literal_Ptr_Type is access all Literal_Type;
   procedure Put(Tree : access Literal_Type);
   procedure Type_Check(Tree : access Literal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Literal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Find_Value_Of(Tree : access Literal_Type;
                           Value : out Real);
   procedure Finish(Tree : access Literal_Type);

   ---------------------------------------------------------------------

   -- Representation for the right hand side of a variable definition
   -- that has zero or more formal parameters.
   type Lambda_Type is new Node_Type with
      record
         Id          : String_Handle_Type;
         Formals     : Node_List_Type;
         Description : Node_Ptr_Type := Null_Node_Ptr;
      end record;

   type Lambda_Ptr_Type is access all Lambda_Type;
   procedure Put(Tree : access Lambda_Type);
   procedure Type_Check(Tree : access Lambda_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Lambda_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Find_Value_Of(Tree : access Lambda_Type;
                           Value : out Real);

   ---------------------------------------------------------------------

   type Formal_Type is new Node_Type with record
      Id : String_Handle_Type;
   end record;

   type Formal_Ptr_Type is access all Formal_Type;
   procedure Put(Tree : access Formal_Type);
   procedure Type_Check(Tree : access Formal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Formal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);

   ---------------------------------------------------------------------

   -- Representation for a single definition with Id on left hand side
   -- and Lambda of zero or more formal parameters on right.
   type Def_Type is new Node_Type with record
      Id : String_Handle_Type;
      Lambda : Node_Ptr_Type := Null_Node_Ptr;
   end record;

   type Def_Ptr_Type is access all Def_Type;
   procedure Put(Tree : access Def_Type);
   procedure Type_Check(Tree : access Def_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Def_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);

   -- Convenience function to access the def's lambda description (body).
   function Def_Lambda_Description(Def : in Node_Ptr_Type) return Node_Ptr_Type;
   ---------------------------------------------------------------------

   -- Representation for an identifier that references a definition id
   -- or a formal parameter.
   type Id_Ref_Type is new Node_Type with
      record
         Id : String_Handle_Type;
         Actuals : Node_List_Type;
      end record;

   type Id_Ref_Ptr_Type is access all Id_Ref_Type;
   procedure Put(Tree : access Id_Ref_Type);
   procedure Type_Check(Tree : access Id_Ref_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Id_Ref_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);

   ---------------------------------------------------------------------

   -- Representation of a named instance.
   type Instance_Type is new Node_Type with record
      Id : String_Handle_Type;
      Description : Node_Ptr_Type;
   end record;

   type Instance_Ptr_Type is access all Instance_Type;
   procedure Put(Tree : access Instance_Type);
   procedure Type_Check(Tree : access Instance_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Instance_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Instance_Type);

   ---------------------------------------------------------------------

   -- The syntax tree (graph) root, which holds all definitions and
   -- instances.
   type Model_Type is new Node_Type with record
      Defs : Node_List_Type;
      Instances : Node_List_Type;
   end record;

   type Model_Ptr_Type is access all Model_Type;
   procedure Put(Tree : access Model_Type);
   procedure Type_Check(Tree : access Model_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type);
   procedure Expand(Tree : access Model_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type);
   procedure Finish(Tree : access Model_Type);

   ---------------------------------------------------------------------

   -- Put a tree of any type to standard output as XML.
   procedure Put_As_XML(Tree : access Node_Type'Class);

   ---------------------------------------------------------------------

   -- Convenience functions make life easier for users of syntax trees.
   procedure Find_Friend_Speed(Friend : in Friend_Ptr_Type;
                               Value : out Real);
   procedure Find_Friend_Vulnerability(Friend : in Friend_Ptr_Type;
                                       Value : out Real);
   procedure Find_Friend_Effectiveness(Friend : in Friend_Ptr_Type;
                                       Value : out Real);
   procedure Find_Trip_Schedule_Start(Trip : in Trip_Ptr_Type;
                                      Value : out Real);
   procedure Find_Trip_Schedule_Interval(Trip : in Trip_Ptr_Type;
                                         Value : out Real);
   procedure Find_Threat_Schedule_Start(Threat : in Threat_Ptr_Type;
                                        Value : out Real);
   procedure Find_Threat_Schedule_Interval(Threat : in Threat_Ptr_Type;
                                           Value : out Real);
   procedure Find_Threat_Vulnerability(Threat : in Threat_Ptr_Type;
                                       Value : out Real);
   procedure Find_Threat_Effectiveness(Threat : in Threat_Ptr_Type;
                                       Value : out Real);
   procedure Find_Threat_Duration(Threat : in Threat_Ptr_Type;
                                  Value : out Real);

end AST.Trees;
