-- Model IED Simulator
-- COL Gene Ressler, 1 February 2009

with Ada.Text_IO, Ada.Characters.Handling, Ada.Containers, Ada.Strings.Unbounded;
use  Ada.Text_IO, Ada.Characters.Handling, Ada.Containers, Ada.Strings.Unbounded;

with AST.Tables;
use  AST.Tables;

package body AST.Trees is

   use Node_Lists;

   procedure Type_Check(Tree : access Geopoint_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      Type_Check(Tree.X, Symbol_Table);
      Type_Check(Tree.Y, Symbol_Table);
      if Tree.X.Tag /= Constant_Number_Tag then
	 Tree.Tag := Error_Tag;
	 raise Type_Error with "X coordinate of geopoint is not a constant number.";
      end if;
      if Tree.Y.Tag /= Constant_Number_Tag then
	 Tree.Tag := Error_Tag;
	 raise Type_Error with "Y coordinate of geopoint is not a constant number.";
      end if;
      Tree.Tag := Geopoint_Tag;
   end Type_Check;

   procedure Type_Check(Tree : access Segment_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Route_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
      C : Cursor;
   begin
      -- Type check all the segments recursively.
      C := First(Tree.Segments);
      while C /= No_Element loop
	 Type_Check(Element(C), Symbol_Table);
	 if Element(C).Tag /= Segment_Tag then
	    Tree.Tag := Error_Tag;
	    raise Type_Error with "Route has bad segment";
	 end if;
         C := Next(C);
      end loop;
      Tree.Tag := Route_Tag;
   end Type_Check;

   procedure Type_Check(Tree : access Friend_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Sensor_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Schedule_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Trip_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Threat_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Add_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Sub_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Mul_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Div_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Neg_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Uniform_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Exponential_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Normal_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Literal_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Lambda_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Formal_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Def_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Id_Ref_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Instance_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   procedure Type_Check(Tree : access Model_Type;
                        Symbol_Table : in Symbol_Table_Ptr_Type) is
   begin
      null;
   end Type_Check;

   -- Expand all the nodes pointed to by a node list and build a
   -- new node list for the results.
   procedure Expand(List : in Node_List_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_List : out Node_List_Type) is
      C : Cursor;
      Expanded_Tree : Node_Ptr_Type;
   begin
      C := First(List);
      while C /= No_Element loop
         Expand(Element(C), Symbol_Table, Expanded_Tree);
         Append(Expanded_List, Expanded_Tree);
         C := Next(C);
      end loop;
   end Expand;

   -- Traverse a given list of defs, which are (id, lambda) pairs.  Add
   -- these pairs to the symbol table. Then traverse again and expand the
   -- definitions with no parameters, using the symbol table for lookups.
   -- Accumulate a new list of defs including only those with no parameters.
   -- This list isn't used for simulation, but supports animation and debugging.
   procedure Bind_Defs(Defs : in Node_List_Type;
                       Symbol_Table : in Symbol_Table_Ptr_Type;
                       New_Defs : out Node_List_Type) is
      C : Cursor;
      Def : Def_Ptr_Type;
      Lambda : Lambda_Ptr_Type;
   begin
      C  := First(Defs);
      while C /= No_Element loop
         Def := Def_Ptr_Type(Element(C));
         Insert(Symbol_Table, Def.Id, Def.Lambda);
         C := Next(C);
      end loop;
      C  := First(Defs);
      while C /= No_Element loop
         Def := Def_Ptr_Type(Element(C));
         Lambda := Lambda_Ptr_Type(Def.Lambda);
         if Length(Lambda.Formals) = 0 then
            Expand(Lambda.Description, Symbol_Table, Lambda.Description);
            Append(New_Defs, Element(C));
         end if;
         C := Next(C);
      end loop;
   end Bind_Defs;

   -- Build a new symbol table that's linked to the previous one.  Insert
   -- mappings for a list of formal parameters to a corresponding list
   -- of actual parameters.
   procedure Bind_Actuals(Symbol_Table : in Symbol_Table_Ptr_Type;
                          Formals : in Node_List_Type;
                          Actuals : in Node_List_Type;
                          New_Symbol_Table : out Symbol_Table_Ptr_Type) is
      C_Formal, C_Actual : Cursor;
   begin
      New_Symbol_Table := new Symbol_Table_Type;
      New_Symbol_Table.Prev := Symbol_Table;
      C_Formal := First(Formals);
      C_Actual := First(Actuals);
      while C_Formal /= No_Element and C_Actual /= No_Element loop
         Insert(New_Symbol_Table,
                Formal_Ptr_Type(Element(C_Formal)).Id,
                Element(C_Actual));
         -- Copy the recorded tag of the formal to a constraint on the
         -- value bound to the formal.
         C_Formal := Next(C_Formal);
         C_Actual := Next(C_Actual);
      end loop;
      if C_Formal /= No_Element then
         raise Type_Error with "More formals than actuals in call.";
      end if;
      if C_Actual /= No_Element then
         raise Type_Error with "More actuals than formals in call.";
      end if;
   end Bind_Actuals;

   ---------------------------------------------------------------------
   ---- Overrides of Node_Type primitive Type_Check
   ---------------------------------------------------------------------

   -- All these procedures follow the same outline.  For nodes with children,
   -- Type_Check them first.  They leave the correct type of the node stored
   -- in Tag fields of the child nodes.  Then use these Tag fields to determine
   -- correct type of the current node.  If the correct type is Type_Error,
   -- then raise a type error exception.

   ---------------------------------------------------------------------
   ---- Overrides of Node_Type primitive Expand
   ---------------------------------------------------------------------

   -- All these procedures follow the same outline.  Allocate a new node
   -- of the same type as the input tree.  Copy non-tree fields (like Ids).
   -- Expand subtrees of the input tree to form subtrees of the new node.
   -- Return the new node.  Where this pattern varies, I'll add special notes.
   procedure Expand(Tree : access Geopoint_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Geopoint_Ptr_Type;
   begin
      P := new Geopoint_Type;
      P.Id := Tree.Id;
      Expand(Tree.X, Symbol_Table, P.X);
      Expand(Tree.Y, Symbol_Table, P.Y);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Segment_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Segment_Ptr_Type;
   begin
      P := new Segment_Type;
      Expand(Tree.A, Symbol_Table, P.A);
      Expand(Tree.B, Symbol_Table, P.B);
      Expand(Tree.Trafficability, Symbol_Table, P.Trafficability);
      Expand(Tree.Vulnerability, Symbol_Table, P.Vulnerability);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Route_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Route_Ptr_Type;
   begin
      P := new Route_Type;
      Expand(Tree.Segments, Symbol_Table, P.Segments);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Friend_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Friend_Ptr_Type;
   begin
      P := new Friend_Type;
      Expanded_Tree := Node_Ptr_Type(P);
      Expand(Tree.Speed, Symbol_Table, P.Speed);
      Expand(Tree.Vulnerability, Symbol_Table, P.Vulnerability);
      Expand(Tree.Effectiveness, Symbol_Table, P.Effectiveness);
      Expand(Tree.Sensor, Symbol_Table, P.Sensor);
   end Expand;

   procedure Expand(Tree : access Sensor_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Sensor_Ptr_Type;
   begin
      P := new Sensor_Type;
      Expand(Tree.The_Range, Symbol_Table, P.The_Range);
      Expand(Tree.Responders, Symbol_Table, P.Responders);
      Expand(Tree.Effectiveness, Symbol_Table, P.Effectiveness);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Schedule_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Schedule_Ptr_Type;
   begin
      P := new Schedule_Type;
      Expand(Tree.Start, Symbol_Table, P.Start);
      Expand(Tree.Interval, Symbol_Table, P.Interval);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Trip_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Trip_Ptr_Type;
   begin
      P := new Trip_Type;
      Expand(Tree.Friend, Symbol_Table, P.Friend);
      Expand(Tree.Route, Symbol_Table, P.Route);
      Expand(Tree.Schedule, Symbol_Table, P.Schedule);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Threat_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Threat_Ptr_Type;
   begin
      P := new Threat_Type;
      Expand(Tree.Target_Segments, Symbol_Table, P.Target_Segments);
      Expand(Tree.Effectiveness, Symbol_Table, P.Effectiveness);
      Expand(Tree.Vulnerability, Symbol_Table, P.Vulnerability);
      Expand(Tree.Schedule, Symbol_Table, P.Schedule);
      Expand(Tree.Duration, Symbol_Table, P.Duration);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Add_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Add_Ptr_Type;
   begin
      P := new Add_Type;
      Expand(Tree.Lhs, Symbol_Table, P.Lhs);
      Expand(Tree.Rhs, Symbol_Table, P.Rhs);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Sub_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Sub_Ptr_Type;
   begin
      P := new Sub_Type;
      Expand(Tree.Lhs, Symbol_Table, P.Lhs);
      Expand(Tree.Rhs, Symbol_Table, P.Rhs);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Mul_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Mul_Ptr_Type;
   begin
      P := new Mul_Type;
      Expand(Tree.Lhs, Symbol_Table, P.Lhs);
      Expand(Tree.Rhs, Symbol_Table, P.Rhs);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Div_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Div_Ptr_Type;
   begin
      P := new Div_Type;
      Expand(Tree.Lhs, Symbol_Table, P.Lhs);
      Expand(Tree.Rhs, Symbol_Table, P.Rhs);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Neg_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Neg_Ptr_Type;
   begin
      P := new Neg_Type;
      Expand(Tree.X, Symbol_Table, P.X);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Uniform_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Uniform_Ptr_Type;
   begin
      P := new Uniform_Type;
      Expand(Tree.Min, Symbol_Table, P.Min);
      Expand(Tree.Max, Symbol_Table, P.Max);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Exponential_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Exponential_Ptr_Type;
   begin
      P := new Exponential_Type;
      Expand(Tree.Beta, Symbol_Table, P.Beta);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Normal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Normal_Ptr_Type;
   begin
      P := new Normal_Type;
      Expand(Tree.Mu, Symbol_Table, P.Mu);
      Expand(Tree.Sigma, Symbol_Table, P.Sigma);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   procedure Expand(Tree : access Literal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Literal_Ptr_Type;
   begin
      P := new Literal_Type;
      P.Value := Tree.Value;
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   -- The following three should never be called because lambdas, defs,
   -- and formal parameters are never expanded.  Just raise an exception.
   procedure Expand(Tree : access Lambda_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
   begin
      raise Constraint_Error with "attempted to Expand a lambda";
   end Expand;

   procedure Expand(Tree : access Formal_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
   begin
      raise Constraint_Error with "attempted to Expand a formal";
   end Expand;

   procedure Expand(Tree : access Def_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
   begin
      raise Constraint_Error with "attempted to Expand a def";
   end Expand;

   -- This one is special because it's where ids are expanded into
   -- the subtrees they represent, including actual parameters.
   procedure Expand(Tree : access Id_Ref_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      Value : Node_Ptr_Type;
      Lambda : Lambda_Ptr_Type;
      Expanded_Actuals : Node_List_Type;
      New_Symbol_Table : Symbol_Table_Ptr_Type;
   begin
      -- Look up the id in the symbol table and get its value.
      Look_Up(Symbol_Table, Tree.Id, Value);

      -- If the value is not a lambda, use it as-is.  No need to expand it.
      -- Currently (10 Jan 2008), this only happens if the id is a formal
      -- parameter that's bound to an actual in the symbol table.  The
      -- actual has already been expanded, so we're done.
      if Value.all not in Lambda_Type then
         Expanded_Tree := Value;
         return;
      end if;

      -- If the value is a lambda with no formals, then it serves as a box.
      -- Return the box as-is. A box is just a link (pointer) to real code.
      -- If we have a pointer to the box, we can change the box's contents,
      -- which is useful. Not expanding box contents is important! It
      -- breaks mutual dependencies, which are cycles in the syntax graph.
      -- Otherwise Expand would recur infinitely. So you can think of boxes
      -- as the base cases. The code in the box will be expanded by the top
      -- level loop that's working on the model's list of defs.
      Lambda := Lambda_Ptr_Type(Value);
      if Length(Lambda.Formals) = 0 then
         Expanded_Tree := Value;
         return;
      end if;

      -- Where the action happens...
      -- If the value is a lambda with parameters, then expand all the actuals.
      -- Bind them to the formals of the lambda in a new symbol table.
      -- Then expand the description of the lambda using the new symbol table.
      -- When references to formals occur in the description, they'll be
      -- correctly looked up in this new symbol table!  Finally, free storage
      -- for the new symbol table; it's no longer needed.
      Expand(Tree.Actuals, Symbol_Table, Expanded_Actuals);
      Bind_Actuals(Symbol_Table, Lambda.Formals, Expanded_Actuals,
                   New_Symbol_Table);
      Expand(Lambda.Description, New_Symbol_Table, Expanded_Tree);
      Free(New_Symbol_Table);
   end Expand;

   procedure Expand(Tree : access Instance_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Instance_Ptr_Type;
   begin
      P := new Instance_Type;
      P.Id := Tree.Id;
      Expand(Tree.Description, Symbol_Table, P.Description);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   -- To process a model, add the Def id->description bindings to the symbol
   -- table, then expand the instances using these bindings.
   procedure Expand(Tree : access Model_Type;
                    Symbol_Table : in Symbol_Table_Ptr_Type;
                    Expanded_Tree : out Node_Ptr_Type) is
      P : Model_Ptr_Type;
   begin
      P := new Model_Type;
      Bind_Defs(Tree.Defs, Symbol_Table, P.Defs);
      Expand(Tree.Instances, Symbol_Table, P.Instances);
      Expanded_Tree := Node_Ptr_Type(P);
   end Expand;

   -- Return the value of a geopoint tree as a 2d point.  This uses the
   -- Ada 2005 aggregate return statement, which is a clean way to return
   -- a record value (and allows limited types to be returned, too, though
   -- that's not the case here).
   function Value_Of(Geopoint : Geopoint_Ptr_Type) return Point_2d_Type is
   begin
      return Value : Point_2d_Type do
         Find_Value_Of(Geopoint.X, Value.X);
         Find_Value_Of(Geopoint.Y, Value.Y);
      end return;
   end Value_Of;

   -- Find the numerical value of trees that are numbers!  Similar to
   -- tinylanguage studied earlier.
   procedure Find_Value_Of(Tree : access Add_Type;
                           Value : out Real) is
      Lhs, Rhs : Real;
   begin
      Find_Value_Of(Tree.Lhs, Lhs);
      Find_Value_Of(Tree.Rhs, Rhs);
      Value := Lhs + Rhs;
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Sub_Type;
                           Value : out Real) is
      Lhs, Rhs : Real;
   begin
      Find_Value_Of(Tree.Lhs, Lhs);
      Find_Value_Of(Tree.Rhs, Rhs);
      Value := Lhs - Rhs;
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Mul_Type;
                           Value : out Real) is
      Lhs, Rhs : Real;
   begin
      Find_Value_Of(Tree.Lhs, Lhs);
      Find_Value_Of(Tree.Rhs, Rhs);
      Value := Lhs * Rhs;
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Div_Type;
                           Value : out Real) is
      Lhs, Rhs : Real;
   begin
      Find_Value_Of(Tree.Lhs, Lhs);
      Find_Value_Of(Tree.Rhs, Rhs);
      Value := Lhs / Rhs;
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Neg_Type;
                           Value : out Real) is
      X : Real;
   begin
      Find_Value_Of(Tree.X, X);
      Value := -X;
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Uniform_Type;
                           Value : out Real) is
   begin
      Get_Random(Tree.Gen.all, Value);
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Exponential_Type;
                           Value : out Real) is
   begin
      Get_Random(Tree.Gen.all, Value);
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Normal_Type;
                           Value : out Real) is
   begin
      Get_Random(Tree.Gen.all, Value);
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Literal_Type;
                           Value : out Real) is
   begin
      Value := Tree.Value;
   end Find_Value_Of;

   procedure Find_Value_Of(Tree : access Lambda_Type;
                           Value : out Real) is
   begin
      Find_Value_Of(Tree.Description, Value);
   end Find_Value_Of;

   ---------------------------------------------------------------------
   ---- Convenience procedures for Finish
   ---------------------------------------------------------------------

   function Image(Tag : in Tag_Type) return String is
      Tag_Image : constant String := Tag_Type'Image(Tag);
   begin
      return To_Lower(Tag_Image(Tag_Image'First .. Tag_Image'Last - 4));
   end Image;

   procedure Merge_Tags(T1, T2 : in out Tag_Type) is
   begin
      if T1 = Null_Tag then
         T1 := T2;
      elsif T2 = Null_Tag then
         T2 := T1;
      elsif T1 /= T2 then
         raise Type_Error
           with "Required " & Image(T1) & ", found " & Image(T2);
      end if;
   end Merge_Tags;

   -- Remove a box (lambda with zero formal parameters) from the
   -- syntax tree by changing the pointer to the box to the box's
   -- description.  This should never be called on a lambda with parameters.
   -- The number of visits prevents infinite recursion due to cycles in
   -- the syntax graph.
   procedure Unbox(Tree : in out Node_Ptr_Type) is
      Lambda : Lambda_Ptr_Type;
   begin
      while Tree.all in Lambda_Type loop
         Lambda := Lambda_Ptr_Type(Tree);
         pragma Assert(Length(Lambda.Formals) = 0, "bad box");
         Tree := Lambda.Description;
      end loop;
      if Tree.N_Visits = 0 then
         Tree.N_Visits := Tree.N_Visits + 1;
         Finish(Tree);
      end if;
   end Unbox;

   -- Remove all the boxes pointed to by a node list by unboxing each
   -- list element in sequence.
   procedure Unbox(List : in out Node_List_Type) is
      C : Cursor;
   begin
      C := First(List);
      while C /= No_Element loop
         Update_Element(List, C, Unbox'Access);
         C := Next(C);
      end loop;
   end Unbox;

   -- Append a new point to a hard route.
   procedure Append_Point(Hard_Route : in out Hard_Route_Type;
                          Geopoint : in Geopoint_Ptr_Type;
                          Trafficability, Vulnerability : Real := 0.0) is
   begin
      Hard_Route_Vectors.Append(Hard_Route,
        Route_Point_Type'(Trafficability => Trafficability,
                          Vulnerability => Vulnerability,
                          Point => Value_Of(Geopoint)));
   end Append_Point;

   -- Find a numeric value of a tree that is either of type number
   -- or a null tree.  In the latter case, the value returned is the
   -- given default. It is assumed that the Tree is unboxed.
   procedure Find_Value_With_Default(Tree : in Node_Ptr_Type;
                                     Default : in Real;
                                     Value : out Real) is
   begin
      pragma Assert(Tree.N_Visits > 0, "find default of unfinished tree");
      if Tree.all in Null_Type then
         Value := Default;
      else
         Find_Value_Of(Tree, Value);
      end if;
   end Find_Value_With_Default;

   -- Find a numeric value of a tree that is either of type number
   -- or a null tree.  In the latter case, the value returned is the
   -- given default. It is assumed that the Tree is unboxed.
   procedure Find_Positive_Value(Tree : in Node_Ptr_Type;
                                 Value : out Real) is
   begin
      pragma Assert(Tree.N_Visits > 0, "find positive of unfinished tree");
      if Tree.all in Null_Type then
         Value := 0.0;
      else
         Find_Value_Of(Tree, Value);
         Value := Real'Max(Value, Real'Model_Small);
      end if;
   end Find_Positive_Value;

   -- Initialize a hard route by interpreting the segments in a syntactic
   -- route in sequence.  "Negated" segments must be properly handled.
   procedure Initialize(Segments : in Node_List_Type;
                        Hard_Route : out Hard_Route_Type) is
      C : Cursor;
      Trafficability, Vulnerability : Real;
      Segment : Segment_Ptr_Type;
      A, B : Geopoint_Ptr_Type;
   begin
      C := First(Segments);
      while C /= No_Element loop
         if Element(C).all in Segment_Type then
            Segment := Segment_Ptr_Type(Element(C));
            A := Geopoint_Ptr_Type(Segment.A);
            B := Geopoint_Ptr_Type(Segment.B);
         else
            -- Must be a negated segment.  Get segment and swap end points.
            Segment := Segment_Ptr_Type(Neg_Ptr_Type(Element(C)).X);
            A := Geopoint_Ptr_Type(Segment.B);
            B := Geopoint_Ptr_Type(Segment.A);
         end if;
         Find_Value_With_Default(Segment.Trafficability, 1.0, Trafficability);
         Find_Value_With_Default(Segment.Vulnerability, 1.0, Vulnerability);
         Append_Point(Hard_Route, A, Trafficability, Vulnerability);
         C := Next(C);
      end loop;
      -- Append the final route point.
      Append_Point(Hard_Route, B);
   end Initialize;

   -- Append a new point to a hard route.
   procedure Append_Segment(Segment_Set : in out Segment_Set_Type;
                            Segment : in Segment_Ptr_Type) is
      Segment_Set_Element : Segment_Set_Element_Type;
   begin
      Segment_Vectors.Append(Segment_Set,
        Segment_Set_Element_Type'(Value_Of(Geopoint_Ptr_Type(Segment.A)),
                                  Value_Of(Geopoint_Ptr_Type(Segment.B))));
   end Append_Segment;

   procedure Append_Route_Segments(Segment_Set : in out Segment_Set_Type;
                                   Route : in Route_Ptr_Type) is
      C : Cursor;
   begin
      C := First(Route.Segments);
      while C /= No_Element loop
         if Element(C).all in Segment_Type then
            Append_Segment(Segment_Set, Segment_Ptr_Type(Element(C)));
         else
            Append_Segment(Segment_Set,
                           Segment_Ptr_Type(Neg_Ptr_Type(Element(C)).X));
         end if;
         C := Next(C);
      end loop;
   end Append_Route_Segments;

   procedure Initialize(Target_Segments : in Node_List_Type;
                        Segment_Set : out Segment_Set_Type) is
      C : Cursor;
   begin
      C := First(Target_Segments);
      while C /= No_Element loop
         if Element(C).all in Segment_Type then
            Append_Segment(Segment_Set, Segment_Ptr_Type(Element(C)));
         else -- must be route
            Append_Route_Segments(Segment_Set, Route_Ptr_Type(Element(C)));
         end if;
         C := Next(C);
      end loop;
   end Initialize;

   ---------------------------------------------------------------------
   ---- Overrides of Node_Type primitive Finish
   ---------------------------------------------------------------------

   procedure Finish(Tree : access Geopoint_Type) is
   begin
      Unbox(Tree.X);
      Unbox(Tree.Y);
   end Finish;

   procedure Finish(Tree : access Segment_Type) is
   begin
      Unbox(Tree.A);
      Unbox(Tree.B);
      Unbox(Tree.Trafficability);
      Unbox(Tree.Vulnerability);
   end Finish;

   procedure Finish(Tree : access Route_Type) is
   begin
      Unbox(Tree.Segments);
      Initialize(Tree.Segments, Tree.Hard_Route);
   end Finish;

   procedure Finish(Tree : access Friend_Type) is
   begin
      Unbox(Tree.Speed);
      Unbox(Tree.Vulnerability);
      Unbox(Tree.Effectiveness);
      Unbox(Tree.Sensor);
   end Finish;

   procedure Finish(Tree : access Sensor_Type) is
   begin
      Unbox(Tree.The_Range);
      Unbox(Tree.Responders);
      Unbox(Tree.Effectiveness);
   end Finish;

   procedure Finish(Tree : access Schedule_Type) is
   begin
      Unbox(Tree.Start);
      Unbox(Tree.Interval);
   end Finish;

   procedure Finish(Tree : access Trip_Type) is
   begin
      Unbox(Tree.Friend);
      Unbox(Tree.Route);
      Unbox(Tree.Schedule);
   end Finish;

   procedure Finish(Tree : access Threat_Type) is
   begin
      Unbox(Tree.Target_Segments);
      Unbox(Tree.Effectiveness);
      Unbox(Tree.Vulnerability);
      Unbox(Tree.Schedule);
      Unbox(Tree.Duration);
      Initialize(Tree.Target_Segments, Tree.Segment_Set);
   end Finish;

   procedure Finish(Tree : access Add_Type) is
   begin
      Unbox(Tree.Lhs);
      Unbox(Tree.Rhs);
   end Finish;

   procedure Finish(Tree : access Sub_Type) is
   begin
      Unbox(Tree.Lhs);
      Unbox(Tree.Rhs);
   end Finish;

   procedure Finish(Tree : access Mul_Type) is
   begin
      Unbox(Tree.Lhs);
      Unbox(Tree.Rhs);
   end Finish;

   procedure Finish(Tree : access Div_Type) is
   begin
      Unbox(Tree.Lhs);
      Unbox(Tree.Rhs);
   end Finish;

   procedure Finish(Tree : access Neg_Type) is
   begin
      Unbox(Tree.X);
   end Finish;

   procedure Finish(Tree : access Uniform_Type) is
      Min, Max : Real;
   begin
      Unbox(Tree.Min);
      Find_Value_With_Default(Tree.Min, 0.0, Min);
      Unbox(Tree.Max);
      Find_Value_With_Default(Tree.Max, 1.0, Max);
      Set(Tree.Gen.all, Min, Max);
   end Finish;

   procedure Finish(Tree : access Exponential_Type) is
      Beta : Real;
   begin
      Unbox(Tree.Beta);
      Find_Value_With_Default(Tree.Beta, 1.0, Beta);
      Set(Tree.Gen.all, Beta);
   end Finish;

   procedure Finish(Tree : access Normal_Type) is
      Mu, Sigma : Real;
   begin
      Unbox(Tree.Mu);
      Find_Value_With_Default(Tree.Mu, 0.0, Mu);
      Unbox(Tree.Sigma);
      Find_Value_With_Default(Tree.Sigma, 1.0, Sigma);
      Set(Tree.Gen.all, Mu, Sigma);
   end Finish;

   procedure Finish(Tree : access Instance_Type) is
   begin
      Unbox(Tree.Description);
   end Finish;

   procedure Finish(Tree : access Model_Type) is
   begin
      Tree.N_Visits := Tree.N_Visits + 1;
      Unbox(Tree.Instances);
   end Finish;

   procedure Finish(Tree : access Literal_Type) is
   begin
      null;
   end Finish;

   ---------------------------------------------------------------------
   ---- Convenience procedures for putting syntax trees
   ---------------------------------------------------------------------

   -- Print a plain opening tag.
   procedure Open(XML_Tag : in String) is
   begin
      Put("<" & XML_Tag & ">");
   end Open;

   -- Print an opening tag with an attribute for the type tag of a tree node.
   procedure Open(Tree : access Node_Type'Class; XML_Tag : in String) is
   begin
      Put("<" & XML_Tag & " tag=""" & Image(Tree.Tag) & """>");
   end Open;

   -- Print a closing XML tag and a line break in the output.
   procedure Close(XML_Tag : in String) is
   begin
      Put_Line("</" & XML_Tag & ">");
   end Close;

   -- Print a closing XML tag and a line break in the output.
   procedure Close(Tree : access Node_Type'Class; XML_Tag : in String) is
   begin
      Put_Line("</" & XML_Tag & ">");
   end Close;

   -- Print a node list in sequence by dispatching Put recursively.
   procedure Put(List : in Node_List_Type) is
      C: Cursor := First(List);
   begin
      while C /= No_Element loop
         Put(Element(C));
         C := Next(C);
      end loop;
   end Put;

   -- Print a list with enclosing XML tags.
   procedure Put(Tag : in String; List : in Node_List_Type) is
   begin
      Open(Tag);
      Put(List);
      Close(Tag);
   end Put;

   -- Print a string from the string table given its handle.
   procedure Put(H : in String_Handle_Type) is
   begin
      Put(To_String(H));
   end Put;

   -- Same as above but enclose in XML tags.
   procedure Put(Tag : in String; H : in String_Handle_Type) is
   begin
      Open(Tag);
      Put(H);
      Close(Tag);
   end Put;

   -- Print any object with enclosing tags.  This Put is dispatching.
   procedure Put(Tag : in String; Obj : access Node_Type'Class) is
   begin
      Open(Tag);
      Put(Obj);
      Close(Tag);
   end Put;

   -- Put a real number.
   procedure Put_Real(Tag : in String; Val : in Real) is
   begin
      Open(Tag);
      Put(Image(Val));
      Close(Tag);
   end Put_Real;

   -- Put a lambda with the option to _not_ print the lambda description
   -- in order to avoid infinite output length for recursive references.
   procedure Put(Tree : access Lambda_Type;
                 Break_Cycles : in Boolean) is
   begin
      Open(Tree, "lambda_type");
      Put("id", Tree.Id);
      Put("formals", Tree.Formals);
      if not Break_Cycles then
         Put("description", Tree.Description);
      end if;
      Close(Tree, "lambda_type");
   end Put;

   ---------------------------------------------------------------------
   ---- Overrides of Node_Type primitive Put
   ---------------------------------------------------------------------

   procedure Put(Tree : access Geopoint_Type) is
   begin
      Open(Tree, "geopoint_type");
      Put("id", Tree.Id);
      Put("x", Tree.X);
      Put("y", Tree.Y);
      Close(Tree, "geopoint_type");
   end Put;

   procedure Put(Tree : access Segment_Type) is
   begin
      Open(Tree, "segment_type");
      Put("a", Tree.A);
      Put("b", Tree.B);
      Put("trafficability", Tree.Trafficability);
      Put("vulnerability", Tree.Vulnerability);
      Close(Tree, "segment_type");
   end Put;

   procedure Put(Tree : access Route_Type) is
   begin
      Open(Tree, "route_type");
      Put("segments", Tree.Segments);
      Close(Tree, "route_type");
   end Put;

   procedure Put(Tree : access Friend_Type) is
   begin
      Open(Tree, "friend_type");
      Put("speed", Tree.Speed);
      Put("vulnerability", Tree.Vulnerability);
      Put("effectiveness", Tree.Effectiveness);
      Put("sensor", Tree.Sensor);
      Close(Tree, "friend_type");
   end Put;

   procedure Put(Tree : access Sensor_Type) is
   begin
      Open(Tree, "sensor_type");
      Put("range", Tree.The_Range);
      Put("responders", Tree.Responders);
      Put("effectiveness", Tree.Effectiveness);
      Close(Tree, "sensor_type");
   end Put;

   procedure Put(Tree : access Schedule_Type) is
   begin
      Open(Tree, "schedule_type");
      Put("start", Tree.Start);
      Put("interval", Tree.Interval);
      Close(Tree, "schedule_type");
   end Put;

   procedure Put(Tree : access Trip_Type) is
   begin
      Open(Tree, "trip_type");
      Put("friend", Tree.Friend);
      Put("route", Tree.Route);
      Put("schedule", Tree.Schedule);
      Close(Tree, "trip_type");
   end Put;

   procedure Put(Tree : access Threat_Type) is
   begin
      Open(Tree, "threat_type");
      Put("target_segments", Tree.Target_Segments);
      Put("effectiveness", Tree.Effectiveness);
      Put("vulnerability", Tree.Vulnerability);
      Put("schedule", Tree.Schedule);
      Put("duration", Tree.Duration);
      Close(Tree, "threat_type");
   end Put;

   procedure Put(Tree : access Add_Type) is
   begin
      Open(Tree, "add_type");
      Put("lhs", Tree.Lhs);
      Put("rhs", Tree.Rhs);
      Close(Tree, "add_type");
   end Put;

   procedure Put(Tree : access Sub_Type) is
   begin
      Open(Tree, "sub_type");
      Put("lhs", Tree.Lhs);
      Put("rhs", Tree.Rhs);
      Close(Tree, "sub_type");
   end Put;

   procedure Put(Tree : access Mul_Type) is
   begin
      Open(Tree, "mul_type");
      Put("lhs", Tree.Lhs);
      Put("rhs", Tree.Rhs);
      Close(Tree, "mul_type");
   end Put;

   procedure Put(Tree : access Div_Type) is
   begin
      Open(Tree, "div_type");
      Put("lhs", Tree.Lhs);
      Put("rhs", Tree.Rhs);
      Close(Tree, "div_type");
   end Put;

   procedure Put(Tree : access Neg_Type) is
   begin
      Open(Tree, "neg_type");
      Put("x", Tree.X);
      Close(Tree, "neg_type");
   end Put;

   procedure Put(Tree : access Uniform_Type) is
   begin
      Open(Tree, "uniform_type");
      Put("min", Tree.Min);
      Put("max", Tree.Max);
      Close(Tree, "uniform_type");
   end Put;

   procedure Put(Tree : access Exponential_Type) is
   begin
      Open(Tree, "exponential_type");
      Put("beta", Tree.Beta);
      Close(Tree, "exponential_type");
   end Put;

   procedure Put(Tree : access Normal_Type) is
   begin
      Open(Tree, "normal_type");
      Put("mu", Tree.Mu);
      Put("sigma", Tree.Sigma);
      Close(Tree, "normal_type");
   end Put;

   procedure Put(Tree : access Literal_Type) is
   begin
      Open(Tree, "literal_type");
      Put_Real("value", Tree.Value);
      Close(Tree, "literal_type");
   end Put;

   procedure Put(Tree : access Lambda_Type) is
   begin
      Put(Tree => Tree , Break_Cycles => True);
   end Put;

   procedure Put(Tree : access Formal_Type) is
   begin
      Open(Tree, "formal_type");
      Put("id", Tree.Id);
      Close(Tree, "formal_type");
   end Put;

   procedure Put(Tree : access Def_Type) is
   begin
      Open(Tree, "def_type");
      Put("id", Tree.Id);
      Put(Tree => Lambda_Ptr_Type(Tree.Lambda), Break_Cycles => False);
      Close(Tree, "def_type");
   end Put;

   procedure Put(Tree : access Id_Ref_Type) is
   begin
      Open(Tree, "id_ref_type");
      Put("id", Tree.Id);
      Put("actuals", Tree.Actuals);
      Close(Tree, "id_ref_type");
   end Put;

   procedure Put(Tree : access Instance_Type) is
   begin
      Open(Tree, "instance_type");
      Put("id", Tree.Id);
      Put("def", Tree.Description);
      Close(Tree, "instance_type");
   end Put;

   procedure Put(Tree : access Model_Type) is
   begin
      Open(Tree, "model");
      Put("defs", Tree.Defs);
      Put("instances", Tree.Instances);
      Close(Tree, "model");
   end Put;

   procedure Put_As_XML(Tree : access Node_Type'Class) is
   begin
      Put_Line("<?xml version=""1.0"" encoding=""ISO-8859-1""?>");
      Put(Tree);
   end Put_As_XML;

   ---------------------------------------------------------------------
   ---- Convenience accessors.
   ---------------------------------------------------------------------

   function Def_Lambda_Description(Def : in Node_Ptr_Type)
                                   return Node_Ptr_Type is
   begin
      return Lambda_Ptr_Type(Def_Ptr_Type(Def).Lambda).Description;
   end Def_Lambda_Description;

   procedure Find_Friend_Speed(Friend : in Friend_Ptr_Type;
                               Value : out Real) is
   begin
      Find_Value_Of (Friend.Speed, Value);
   end Find_Friend_Speed;

   procedure Find_Friend_Vulnerability(Friend : in Friend_Ptr_Type;
                                       Value : out Real) is
   begin
      Find_Value_With_Default(Friend.Vulnerability, 1.0, Value);
   end Find_Friend_Vulnerability;

   procedure Find_Friend_Effectiveness(Friend : in Friend_Ptr_Type;
                                       Value : out Real) is
   begin
      Find_Value_With_Default(Friend.Effectiveness, 1.0, Value);
   end Find_Friend_Effectiveness;

   function Trip_Schedule(Trip : in Node_Ptr_Type) return Schedule_Ptr_Type is
   begin
      return Schedule_Ptr_Type(Trip_Ptr_Type(Trip).Schedule);
   end Trip_Schedule;

   procedure Find_Schedule_Start(Schedule : in Node_Ptr_Type;
                                 Value : out Real) is
   begin
      if Schedule.all in Null_Type then
         Value := 0.0;
      else
         Find_Value_With_Default(Schedule_Ptr_Type(Schedule).Start, 0.0, Value);
         Value := Real'Max(Value, 0.0);
      end if;
   end Find_Schedule_Start;

   procedure Find_Schedule_Interval(Schedule : in Node_Ptr_Type;
                                    Value : out Real) is
   begin
      if Schedule.all in Null_Type then
         Value := 0.0;
      else
         Find_Positive_Value(Schedule_Ptr_Type(Schedule).Interval, Value);
      end if;
   end Find_Schedule_Interval;

   procedure Find_Trip_Schedule_Start(Trip : in Trip_Ptr_Type;
                                      Value : out Real) is
   begin
      Find_Schedule_Start(Trip.Schedule, Value);
   end Find_Trip_Schedule_Start;

   procedure Find_Trip_Schedule_Interval(Trip : in Trip_Ptr_Type;
                                         Value : out Real) is
   begin
      Find_Schedule_Interval(Trip.Schedule, Value);
   end Find_Trip_Schedule_Interval;

   procedure Find_Threat_Schedule_Start(Threat : in Threat_Ptr_Type;
                                        Value : out Real) is
   begin
      Find_Schedule_Start(Threat.Schedule, Value);
   end Find_Threat_Schedule_Start;

   procedure Find_Threat_Schedule_Interval(Threat : in Threat_Ptr_Type;
                                           Value : out Real) is
   begin
      Find_Schedule_Interval(Threat.Schedule, Value);
   end Find_Threat_Schedule_Interval;

   procedure Find_Threat_Vulnerability(Threat : in Threat_Ptr_Type;
                                       Value : out Real) is
   begin
      Find_Value_With_Default(Threat.Vulnerability, 1.0, Value);
   end Find_Threat_Vulnerability;

   procedure Find_Threat_Effectiveness(Threat : in Threat_Ptr_Type;
                                       Value : out Real) is
   begin
      Find_Value_With_Default(Threat.Effectiveness, 1.0, Value);
   end Find_Threat_Effectiveness;

   procedure Find_Threat_Duration(Threat : in Threat_Ptr_Type;
                                  Value : out Real) is
   begin
      Find_Positive_Value(Threat.Duration, Value);
   end Find_Threat_Duration;

end AST.Trees;
