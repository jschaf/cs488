-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

--  1. <model> --> { <description-section> <instance-section> } EOF
--  2. <description-section> --> description { <named-description> }
--  3. <named-description> -->  ID  [ ( <parameter-list> ) ] = <description> ;
--                           |  ID  [ ( <parameter-list> ) ] : <type-id> ;
--  4. <parameter-list> --> <parameter> {, <parameter> }
--  5. <parameter> --> ID : <type-id>
--  6. <type-id> --> number | point | segment | route | friend | schedule
--             | sensor
--  7. <description> --> <expr> | <point-description> | <segment-description>
--                     | <route-description> | <friend-description> |
--                     | <trip-description>  | <threat-description>  |
--                     | <schedule-description> | <sensor-description>
--  8. <point-description> -->  ( <expr> , <expr> )
--  9. <segment-description> --> segment ID -> ID [ <with-attributes> ]
--  10. <route-description> --> route ( <segment-list> )
--  11. <segment-list> --> <segment-id> {, <segment-id> }
--  12. <segment-id> --> ID | ~ ID
--  13. <friend-description> --> friend <expr> [ <with-attributes> ]
--  14. <trip-description> --> trip ID -> ID [ <with-attributes> ]
--  15. <threat-description> --> threat ( <id-list>  ) [ <with-attributes> ]
--  16. <schedule-description> --> schedule  [ <with-attributes> ]
--  17. <sensor-description> --> sensor <expr> ->
--                                  ( <id-list>  ) [ <with-attributes> ]
--  18. <with-attributes> --> with <attribute-list>
--  19. <attribute-list> --> <attribute-pair> {, <attribute-pair> }
--  20. <attribute-pair> --> <expr-attribute-name> = <expr>
--  21. <id-list> --> ID { , ID }
--  22. <instance-section> --> instance { <instance> }
--  23. <instance> --> ID : ID  [ ( <expr-list>  ) ] ;
--  24. <expr-list> --> <expr> { , <expr> }
--  25. <expr-attribute-name> --> trafficability | vulnerability | range
--                         | effectiveness | schedule | start | interval
--  26. <expr> --> <term> { <addop> <term> }
--  27. <term> --> <signed-factor> { <mulop> <signed-factor> }
--  28. <signed-factor> --> - <factor> | <factor>
--  29. <factor> --> ( <expr> )
--                 | <random-var>
--                 | ID [ ( <expr-list>  ) ]
--                 | NUMBER
--  30. <random-var> --> uniform ( <expr> , <expr> )
--                     | normal ( <expr> , <expr> )
--                     | exponential ( <expr> )
--  31. <addop> --> + | -
--  32. <mulop> --> * | /

with Ada.Text_IO, Ada.Characters.Handling, Ada.Strings.Fixed;
use  Ada.Text_IO, Ada.Characters.Handling, Ada.Strings.Fixed;

with Common, String_Table, Scanner, AST.Tables, AST.Trees;
use  Common, String_Table, Scanner, AST.Tables, AST.Trees;

package body Parser is

   use Node_Lists;

   -- Convenience procedure to append a pointer to any kind of node
   -- to a node list without casting it to Node_Ptr_Type.
   procedure Append_Node(List : in out Node_List_Type;
                         Item : access Node_Type'Class) is
   begin
      Append(List, Node_Ptr_Type(Item));
   end Append_Node;

   procedure Parse
     (S : in String;
      Rtn_Tree : out Node_Ptr_Type;
      Debug_Mode : in Boolean := False)
   is

      -- A debugging print routine with indentation control.
      -- If first character of Msg is '+', indentation is increased by one
      -- level after printing the message.  If it's '-', indentation is
      -- decreased by one level before printing.
      Indentation : Natural := 0;
      procedure Debug (Msg : in String) is
      begin
         if Debug_Mode then
            if Msg (Msg'First) = '-' then
               Indentation := Indentation - 1;
            end if;
            Put_Line (Indentation * ' ' & Msg);
            if Msg (Msg'First) = '+' then
               Indentation := Indentation + 1;
            end if;
         end if;
      end Debug;

      procedure Debug_Token(Start_Index : in Positive;
                            End_Index : in Natural;
                            Line_Number : in Positive;
                            Token : in Token_Type) is
      begin
         if Debug_Mode then
            Put_Scanned_Token(S, Start_Index, End_Index, Line_Number,
                              Token, Indentation);
            New_Line;
         end if;
      end Debug_Token;

      -- These describe the current lookahead token.
      Start_Index : Positive := 1;  -- Start of token string.
      End_Index : Natural := 0;  -- End of token string.
      Line_Number : Positive := 1;
      Look_Ahead : Token_Type := End_Input; -- Enumerated type of token.

      -- Return the current lookahead token string by slicing the input.
      function Token_String return String is
      begin
         return S (Start_Index .. End_Index);
      end Token_String;

      -- Scan the next token into the lookahead and print a description if
      -- in debug mode.
      procedure Advance is
      begin
         Scan_Next_Token (S, Start_Index, End_Index, Line_Number, Look_Ahead);
         Debug_Token(Start_Index, End_Index, Line_Number, Look_Ahead);
      end Advance;

      -- Make sure the next token in the lookahead matches one we specify.  If
      -- not, signal a syntax error.
      procedure Match (Token : in Token_Type) is
      begin
         if Look_Ahead = Token then
            Advance;
         else
            raise Syntax_Error with "expected a " &
                                    To_Lower (Token_Type'Image (Token));
         end if;
      end Match;

      ------------------------------------------------------------------
      ---- Optional attributes and tables for holding them
      ------------------------------------------------------------------

      -- All possible optional attributes.
      type Attribute_Type is
        (Attr_Null,
         Attr_Duration, Attr_Interval, Attr_Effectiveness,
         Attr_Range, Attr_Schedule, Attr_Sensor, Attr_Start,
         Attr_Trafficability, Attr_Vulnerability);

      -- An attribute-value pair for our tables.
      type Attribute_Table_Entry_Type is
         record
            Attr : Attribute_Type := Attr_Null;
            Value : Node_Ptr_Type := Null_Node_Ptr;
         end record;

      -- Tables of attribute-value pairs of arbitrary length.
      type Attribute_Table_Type is
        array (Positive range <>) of Attribute_Table_Entry_Type;

      -- See the With_Attributes parsing procedure.
      -- To process optional attributes, we call With_Attributes, passing it
      -- a table where the Attr fields have been filled in with the valid
      -- attributes we're looking for.  After an attribute keyword and
      -- corresponding value (an expr) are parsed, we look up the corresonding
      -- attribute in the table.  When found, the value is inserted.  If not
      -- found, the user has specified an attribute not expected for this
      -- kind of object, so raise an exception.  This is actually semantic
      -- analysis mixed in with the parsing!
      procedure Insert(Table : in out Attribute_Table_Type;
                       Attr : in Attribute_Type;
                       Value : in Node_Ptr_Type) is
      begin
         for I in Table'Range loop
            if Table(I).Attr = Attr then
               Table(I).Value := Value;
               return;
            end if;
         end loop;
         raise Syntax_Error
           with "unexpected attribute " & Attribute_Type'Image(Attr);
      end Insert;

      procedure Look_Up(Table : in Attribute_Table_Type;
                       Attr : in Attribute_Type;
                       Value : out Node_Ptr_Type) is
      begin
         for I in Table'Range loop
            if Table(I).Attr = Attr then
               Value := Table(I).Value;
               return;
            end if;
         end loop;
         -- Not found.  Return a null value.
         Value := Null_Node_Ptr;
      end Look_Up;

      ------------------------------------------------------------------

      procedure Model (Rtn_Tree : out Node_Ptr_Type);
      procedure Description_Section (Rtn_Model : in Model_Ptr_Type);
      procedure Named_Description (Rtn_Def : out Def_Ptr_Type);
      -- procedure Parameter_List;
      procedure Parameter (Rtn_Formal : out Formal_Ptr_Type);
      procedure Type_Id (Rtn_Tag : out Tag_Type);
      procedure Description (Rtn_Desc : out Node_Ptr_Type);
      -- procedure Point_Description;
      procedure Segment_Description (Rtn_Segment : out Node_Ptr_Type);
      procedure Route_Description (Rtn_Route : out Node_Ptr_Type);
      procedure Segment_List (Route : in Route_Ptr_Type);
      -- procedure Segment_Id;
      procedure Friend_Description (Rtn_Friend : out Node_Ptr_Type);
      procedure Trip_Description (Rtn_Trip : out Node_Ptr_Type);
      procedure Threat_Description (Rtn_Threat : out Node_Ptr_Type);
      procedure Schedule_Description (Rtn_Schedule : out Node_Ptr_Type);
      procedure Sensor_Description (Rtn_Sensor : out Node_Ptr_Type);
      procedure With_Attributes (Attribute_Table : in out Attribute_Table_Type);
      procedure Attribute_List (Attribute_Table : in out Attribute_Table_Type);
      procedure Attribute_Pair (Attribute_Table : in out Attribute_Table_Type);
      procedure Instance_Section (Rtn_Model : in Model_Ptr_Type);
      procedure Instance (Rtn_Instance : in Instance_Ptr_Type);
      procedure Expr_Attribute_Name (Attr : out Attribute_Type);
      procedure Expr (Rtn : out Node_Ptr_Type);
      procedure Term (Rtn : out Node_Ptr_Type);
      procedure Signed_Factor (Rtn : out Node_Ptr_Type);
      procedure Factor (Rtn : out Node_Ptr_Type);

      --  1. <model> --> { <description-section> <instance-section> } EOF
      procedure Model (Rtn_Tree : out Node_Ptr_Type) is
         Rtn_Model : Model_Ptr_Type := new Model_Type;
      begin
         Debug ("+Model");
         while Look_Ahead = Keyword_Description loop
            Description_Section(Rtn_Model);
            Instance_Section(Rtn_Model);
         end loop;
         if Look_Ahead /= End_Input then
            raise Syntax_Error
              with "expected end of input";
         end if;
         Rtn_Tree := Node_Ptr_Type(Rtn_Model);
         Debug ("-Model");
      end Model;

      --  2. <description-section> --> description <named-description>
      --                                  { <named-description> }
      procedure Description_Section (Rtn_Model : in Model_Ptr_Type) is
         Def : Def_Ptr_Type;
      begin
         Debug ("+Description_Section");
         Match (Keyword_Description);
         loop
            Named_Description (Def);
            Append_Node(Rtn_Model.Defs, Def);
            exit when Look_Ahead /= Id;
         end loop;
         Debug ("-Description_Section");
      end Description_Section;

      --  3. <named-description> --> ID [(<parameter-list>)] = <description> ;
      --                           |  ID  [ ( <parameter-list> ) ] : <type-id> ;
      procedure Named_Description (Rtn_Def : out Def_Ptr_Type) is
         Formal : Formal_Ptr_Type;
         Lambda : Lambda_Ptr_Type;
      begin
         Debug ("+Named_Description");
         Rtn_Def := new Def_Type;
         Rtn_Def.Id := To_Handle(Token_String);
         Lambda := new Lambda_Type;
         Lambda.Id := Rtn_Def.Id;
         Rtn_Def.Lambda := Node_Ptr_Type(Lambda);
         Match (Id);
         if Look_Ahead = Left_Paren then
            Advance;
            loop
               Parameter(Formal);
               Append_Node(Lambda.Formals, Formal);
               exit when Look_Ahead /= Comma;
               Advance;
            end loop;
            Match (Right_Paren);
         end if;
         if Look_Ahead = Equals then
            Advance;
            Description (Lambda.Description);

            -- Store id in geopoint to maintain logical labels for graph algorithms.
            if Lambda.Description.all in Geopoint_Type then
               Geopoint_Ptr_Type(Lambda.Description).Id := Rtn_Def.Id;
            end if;
         else
            Match(Colon);
            Formal := new Formal_Type;
            Formal.Id := To_Handle("{return}");
            Lambda.Description := Node_Ptr_Type(Formal);
            Type_Id(Formal.Tag);
         end if;
         Match (Semi);

         Debug ("-Named_Description");
      end Named_Description;

      --  5. <parameter> --> ID : <type-id>
      procedure Parameter (Rtn_Formal : out Formal_Ptr_Type) is
      begin
         Debug ("+Parameter");
         Rtn_Formal := new Formal_Type;
         Rtn_Formal.Id := To_Handle(Token_String);
         Match (Id);
         Match (Colon);
         Type_Id(Rtn_Formal.Tag);
         Debug ("-Parameter");
      end Parameter;

      --  6. <type-id> --> number | point | segment | route | friend | schedule
      --             | sensor
      procedure Type_Id (Rtn_Tag : out Tag_Type) is
      begin
         Debug ("+Type_Id");
         case Look_Ahead is
            when Keyword_Number =>
               Rtn_Tag := Number_Tag;
            when Keyword_Point =>
               Rtn_Tag := Geopoint_Tag;
            when Keyword_Segment =>
               Rtn_Tag := Segment_Tag;
            when Keyword_Route =>
               Rtn_Tag := Route_Tag;
            when Keyword_Friend =>
               Rtn_Tag := Friend_Tag;
            when Keyword_Schedule =>
               Rtn_Tag := Schedule_Tag;
            when Keyword_Sensor =>
               Rtn_Tag := Sensor_Tag;
            when Keyword_Trip =>
               Rtn_Tag := Trip_Tag;
            when others =>
               raise Syntax_Error with "Expected type identifier";
         end case;
         Advance;
         Debug ("-Type_Id");
      end Type_Id;

      --  7. <description> --> <expr>
      --                     | <segment-description> -- modified for LL(1)!
      --                     | <route-description> | <friend-description> |
      --                     | <trip-description>  | <threat-description>  |
      --                     | <schedule-description> | <sensor-description>
      procedure Description (Rtn_Desc : out Node_Ptr_Type) is
      begin
         Debug ("+Description");
         case Look_Ahead is
            when Left_Paren          |
                 Minus               |
                 Id                  |
                 Number              |
                 Keyword_Uniform     |
                 Keyword_Exponential |
                 Keyword_Normal      =>
               Expr (Rtn_Desc);
            when Keyword_Segment =>
               Segment_Description (Rtn_Desc);
            when Keyword_Route =>
               Route_Description (Rtn_Desc);
            when Keyword_Friend =>
               Friend_Description (Rtn_Desc);
            when Keyword_Trip =>
               Trip_Description (Rtn_Desc);
            when Keyword_Threat =>
               Threat_Description (Rtn_Desc);
            when Keyword_Schedule =>
               Schedule_Description (Rtn_Desc);
            when Keyword_Sensor =>
               Sensor_Description (Rtn_Desc);
            when others =>
               raise Syntax_Error with "expected start of description";
         end case;
         Debug ("-Description");
      end Description;

      --  9. <segment-description> --> segment ID -> ID [ <with-attributes> ]
      procedure Segment_Description (Rtn_Segment : out Node_Ptr_Type) is
         Segment : Segment_Ptr_Type;
         Id_Ref : Id_Ref_Ptr_Type;
         Attributes : Attribute_Table_Type :=
           ((Attr => Attr_Trafficability, Value => <>),
            (Attr => Attr_Vulnerability,  Value => <>));
      begin
         Debug ("+Segment_Description");
         Match (Keyword_Segment);
         Segment := new Segment_Type;

         -- Point A.
         Id_Ref := new Id_Ref_Type;
         Id_Ref.Id := To_Handle(Token_String);
         Segment.A := Node_Ptr_Type(Id_Ref);
         Match (Id);

         Match (Arrow);

         -- Point B.
         Id_Ref := new Id_Ref_Type;
         Id_Ref.Id := To_Handle(Token_String);
         Segment.B := Node_Ptr_Type(Id_Ref);
         Match (Id);

         With_Attributes(Attributes);
         Look_Up(Attributes, Attr_Trafficability, Segment.Trafficability);
         Look_Up(Attributes, Attr_Vulnerability, Segment.Vulnerability);

         Rtn_Segment := Node_Ptr_Type(Segment);
         Debug ("-Segment_Description");
      end Segment_Description;

      --  10. <route-description> --> route ( <segment-list> )
      procedure Route_Description (Rtn_Route : out Node_Ptr_Type) is
         Route : Route_Ptr_Type;
      begin
         Debug ("+Route_Description");
         Match (Keyword_Route);
         Match (Left_Paren);
         Route := new Route_Type;
         Segment_List(Route);
         Match (Right_Paren);
         Rtn_Route := Node_Ptr_Type(Route);
         Debug ("-Route_Description");
      end Route_Description;

      --  11. <segment-list> --> <segment-id> {, <segment-id> }
      procedure Segment_List (Route : in Route_Ptr_Type) is
         Seg_Ref : Id_Ref_Ptr_Type;
         Neg : Neg_Ptr_Type;
      begin
         Debug ("+Segment_List");
         loop
            --  12. <segment-id> --> ID | ~ ID
            case Look_Ahead is
               when Id =>
                  Seg_Ref := new Id_Ref_Type;
                  Seg_Ref.Id := To_Handle(Token_String);
                  Append_Node(Route.Segments, Seg_Ref);
                  Advance;
               when Tilde =>
                  Advance;
                  Seg_Ref := new Id_Ref_Type;
                  Seg_Ref.Id := To_Handle(Token_String);
                  Neg := new Neg_Type;
                  Neg.X := Node_Ptr_Type(Seg_Ref);
                  Append_Node(Route.Segments, Neg);
                  Match (Id);
               when others =>
                  raise Syntax_Error with "expected segment identifier";
            end case;
            exit when Look_Ahead /= Comma;
            Advance;
         end loop;
         Debug ("-Segment_List");
      end Segment_List;

      --  13. <friend-description> --> friend <expr> [ <with-attributes> ]
      procedure Friend_Description (Rtn_Friend : out Node_Ptr_Type) is
         Friend : Friend_Ptr_Type;
         Attributes : Attribute_Table_Type :=
           ((Attr => Attr_Effectiveness, Value => <>),
            (Attr => Attr_Vulnerability, Value => <>),
            (Attr => Attr_Sensor, Value => <>));
      begin
         Debug ("+Friend_Description");
         Match (Keyword_Friend);
         Friend := new Friend_Type;
         Expr (Friend.Speed);
         With_Attributes(Attributes);
         Look_Up(Attributes, Attr_Effectiveness, Friend.Effectiveness);
         Look_Up(Attributes, Attr_Vulnerability, Friend.Vulnerability);
         Look_Up(Attributes, Attr_Sensor, Friend.Sensor);
         Rtn_Friend := Node_Ptr_Type(Friend);
         Debug ("-Friend_Description");
      end Friend_Description;

      --  14. <trip-description> --> trip ID -> ID [ <with-attributes> ]
      procedure Trip_Description (Rtn_Trip : out Node_Ptr_Type) is
         Trip : Trip_Ptr_Type;
         Ref : Id_Ref_Ptr_Type;
         Attributes : Attribute_Table_Type :=
           (1 => (Attr => Attr_Schedule, Value => <>));
      begin
         Debug ("+Trip_Description");
         Match (Keyword_Trip);
         Trip := new Trip_Type;

         Ref := new Id_Ref_Type;
         Ref.Id := To_Handle(Token_String);
         Trip.Friend := Node_Ptr_Type(Ref);
         Match (Id);

         Match (Arrow);

         Ref := new Id_Ref_Type;
         Ref.Id := To_Handle(Token_String);
         Trip.Route := Node_Ptr_Type(Ref);
         Match (Id);

         With_Attributes(Attributes);
         Look_Up(Attributes, Attr_Schedule, Trip.Schedule);

         Rtn_Trip := Node_Ptr_Type(Trip);
         Debug ("-Trip_Description");
      end Trip_Description;

      --  15. <threat-description> --> threat (<id-list>) [<with-attributes>]
      procedure Threat_Description (Rtn_Threat : out Node_Ptr_Type) is
         Threat : Threat_Ptr_Type;
         Id_Ref : Id_Ref_Ptr_Type;
         Attributes : Attribute_Table_Type :=
           ((Attr => Attr_Duration, Value => <>),
            (Attr => Attr_Schedule, Value => <>),
            (Attr => Attr_Effectiveness, Value => <>),
            (Attr => Attr_Vulnerability, Value => <>));
      begin
         Debug ("+Threat_Description");
         Match (Keyword_Threat);
         Match (Left_Paren);
         Threat := new Threat_Type;
         loop
            Id_Ref := new Id_Ref_Type;
            Id_Ref.Id := To_Handle(Token_String);
            Append_Node(Threat.Target_Segments, Id_Ref);
            Match (Id);
            exit when Look_Ahead /= Comma;
            Advance;
         end loop;
         Match (Right_Paren);

         With_Attributes(Attributes);
         Look_Up(Attributes, Attr_Schedule, Threat.Schedule);
         Look_Up(Attributes, Attr_Duration, Threat.Duration);
         Look_Up(Attributes, Attr_Effectiveness, Threat.Effectiveness);
         Look_Up(Attributes, Attr_Vulnerability, Threat.Vulnerability);

         Rtn_Threat := Node_Ptr_Type(Threat);
         Debug ("-Threat_Description");
      end Threat_Description;

      --  16. <schedule-description> --> schedule  [ <with-attributes> ]
      procedure Schedule_Description (Rtn_Schedule : out Node_Ptr_Type) is
         Schedule : Schedule_Ptr_Type;
         Attributes : Attribute_Table_Type :=
           ((Attr => Attr_Start, Value => <>),
            (Attr => Attr_Interval, Value => <>));
      begin
         Debug ("+Schedule_Description");
         Match (Keyword_Schedule);
         Schedule := new Schedule_Type;

         With_Attributes(Attributes);
         Look_Up(Attributes, Attr_Start, Schedule.Start);
         Look_Up(Attributes, Attr_Interval, Schedule.Interval);

         Rtn_Schedule := Node_Ptr_Type(Schedule);
         Debug ("-Schedule_Description");
      end Schedule_Description;

      --  17. <sensor-description> --> sensor <expr> ->
      --                                  ( <id-list>  ) [ <with-attributes> ]
      procedure Sensor_Description (Rtn_Sensor : out Node_Ptr_Type) is
         Sensor : Sensor_Ptr_Type;
         Id_Ref : Id_Ref_Ptr_Type;
         Attributes : Attribute_Table_Type :=
           (1 => (Attr => Attr_Effectiveness, Value => <>));
      begin
         Debug ("+Sensor_Description");
         Match (Keyword_Sensor);
         Sensor := new Sensor_Type;
         Expr (Sensor.The_Range);
         Match (Arrow);
         Match (Left_Paren);
         loop
            Id_Ref := new Id_Ref_Type;
            Id_Ref.Id := To_Handle(Token_String);
            Append_Node(Sensor.Responders, Id_Ref);
            Match (Id);
            exit when Look_Ahead /= Comma;
            Advance;
         end loop;
         Match (Right_Paren);

         With_Attributes(Attributes);
         Look_Up(Attributes, Attr_Effectiveness, Sensor.Effectiveness);

         Rtn_Sensor := Node_Ptr_Type(Sensor);
         Debug ("-Sensor_Description");
      end Sensor_Description;

      --  18. <with-attributes> --> with <attribute-list>
      procedure With_Attributes (Attribute_Table : in out Attribute_Table_Type) is
      begin
         Debug ("+With_Attributes");
         if Look_Ahead = Keyword_With then
            Advance;
            Attribute_List (Attribute_Table);
         end if;
         Debug ("-With_Attributes");
      end With_Attributes;

      --  19. <attribute-list> --> <attribute-pair> {, <attribute-pair> }
      procedure Attribute_List (Attribute_Table : in out Attribute_Table_Type) is
      begin
         Debug ("+Attribute_List");
         loop
            Attribute_Pair (Attribute_Table);
            exit when Look_Ahead /= Comma;
            Advance;
         end loop;
         Debug ("-Attribute_List");
      end Attribute_List;

      --  20. <attribute-pair> --> <expr-attribute-name> = <expr>
      procedure Attribute_Pair (Attribute_Table : in out Attribute_Table_Type) is
         Attr : Attribute_Type;
         Val : Node_Ptr_Type;
      begin
         Debug ("+Attribute_Pair");
         Expr_Attribute_Name(Attr);
         Match (Equals);
         Expr (Val);
         Insert(Attribute_Table, Attr, Val);
         Debug ("-Attribute_Pair");
      end Attribute_Pair;

      --  22. <instance-section> --> instance <instance> { <instance> }
      procedure Instance_Section (Rtn_Model : in Model_Ptr_Type) is
         The_Instance : Instance_Ptr_Type;
      begin
         Debug ("+Instance_Section");
         Match (Keyword_Instance);
         loop
            The_Instance := new Instance_Type;
            Instance(The_Instance);
            Append_Node(Rtn_Model.Instances, The_Instance);
            exit when Look_Ahead /= Id;
         end loop;
         Debug ("-Instance_Section");
      end Instance_Section;

      --  23. <instance> --> ID : ID  [ ( <expr-list>  ) ] ;
      procedure Instance (Rtn_Instance : in Instance_Ptr_Type) is
         Val : Node_Ptr_Type;
         Id_Ref : Id_Ref_Ptr_Type;
      begin
         Debug ("+Instance");
         Rtn_Instance.Id := To_Handle(Token_String);
         Match (Id);
         Match (Colon);

         Id_Ref := new Id_Ref_Type;
         Rtn_Instance.Description := Node_Ptr_Type(Id_Ref);
         Id_Ref.Id :=  To_Handle(Token_String);
         Match (Id);

         if Look_Ahead = Left_Paren then
            Advance;
            loop
               Expr (Val);
               Append_Node(Id_Ref.Actuals, Val);
               exit when Look_Ahead /= Comma;
               Advance;
            end loop;
            Match (Right_Paren);
         end if;
         Match (Semi);
         Debug ("-Instance");
      end Instance;

      --  25. <expr-attribute-name> --> trafficability | vulnerability | range
      --                         | effectiveness | schedule | start | interval
      Attr_Table : constant array(Attribute_Type) of Keyword_Type :=
        (Attr_Null => Illegal_Token,
         Attr_Duration => Keyword_Duration,
         Attr_Effectiveness => Keyword_Effectiveness,
         Attr_Interval => Keyword_Interval,
         Attr_Range => Keyword_Range,
         Attr_Sensor => Keyword_Sensor,
         Attr_Schedule => Keyword_Schedule,
         Attr_Start => Keyword_Start,
         Attr_Trafficability => Keyword_Trafficability,
         Attr_Vulnerability => Keyword_Vulnerability);

      procedure Expr_Attribute_Name (Attr : out Attribute_Type) is
      begin
         Debug ("+Expr_Attribute_Name");
         Attr := Attr_Null;
         for I in Attr_Table'Range loop
            if Look_Ahead = Attr_Table(I) then
               Attr := I;
               exit;
            end if;
         end loop;
         Advance;
         Debug ("-Expr_Attribute_Name");
      end Expr_Attribute_Name;

      --  26. <expr> --> <term> { <addop> <term> }
      procedure Expr (Rtn : out Node_Ptr_Type) is
      begin
         Debug ("+Expr");
         Term (Rtn);
         while Look_Ahead = Plus or Look_Ahead = Minus loop
            Advance;
            if Look_Ahead = Plus then
               declare
                  Add : Add_Ptr_Type := new Add_Type;
               begin
                  Add.Lhs := Rtn;
                  Term(Add.Rhs);
                  Rtn := Node_Ptr_Type(Add);
               end;
            else
               declare
                  Sub : Sub_Ptr_Type := new Sub_Type;
               begin
                  Sub.Lhs := Rtn;
                  Term(Sub.Rhs);
                  Rtn := Node_Ptr_Type(Sub);
               end;
            end if;
         end loop;
         Debug ("-Expr");
      end Expr;

      --  27. <term> --> <signed-factor> { <mulop> <signed-factor> }
      procedure Term (Rtn : out Node_Ptr_Type) is
      begin
         Debug ("+Term");
         Signed_Factor(Rtn);
         while Look_Ahead = Star or Look_Ahead = Slash loop
            if Look_Ahead = Star then
               Advance;
               declare
                  Mul : Mul_Ptr_Type := new Mul_Type;
               begin
                  Mul.Lhs := Rtn;
                  Signed_Factor(Mul.Rhs);
                  Rtn := Node_Ptr_Type(Mul);
               end;
            else
               Advance;
               declare
                  Div : Div_Ptr_Type := new Div_Type;
               begin
                  Div.Lhs := Rtn;
                  Signed_Factor(Div.Rhs);
                  Rtn := Node_Ptr_Type(Div);
               end;
            end if;
         end loop;
         Debug ("-Term");
      end Term;

      --  28. <signed-factor> --> - <factor> | <factor>
      procedure Signed_Factor (Rtn : out Node_Ptr_Type) is
         Negate : Boolean := False;
         Neg : Neg_Ptr_Type;
      begin
         Debug ("+Signed_Factor");
         if Look_Ahead = Minus then
            Negate := True; -- remember we saw a minus sign
            Advance;
         end if;
         Factor (Rtn);
         if Negate then
            Neg := new Neg_Type;
            Neg.X := Rtn;
            Rtn := Node_Ptr_Type(Neg);
         end if;
         Debug ("-Signed_Factor");
      end Signed_Factor;

      --  29. <factor> --> ( <expr> )
      --                 | <random-var>
      --                 | ID [ ( <expr-list>  ) ]
      --                 | NUMBER
      --                 | ( <expr> , <expr> ) -- Modified for LL(1)!
      procedure Factor (Rtn : out Node_Ptr_Type) is
         Point : Geopoint_Ptr_Type;
         Uniform : Uniform_Ptr_Type;
         Exponential : Exponential_Ptr_Type;
         Normal : Normal_Ptr_Type;
         Id_Ref : Id_Ref_Ptr_Type;
         Actual : Node_Ptr_Type;
         Literal : Literal_Ptr_Type;
      begin
         Debug ("+Factor");
         case Look_Ahead is
            when Left_Paren =>
               Advance;
               Expr (Rtn);
               if Look_Ahead = Comma then
                  Advance;
                  Point := new Geopoint_Type;
                  Point.X := Rtn;
                  Expr (Point.Y);
                  Rtn := Node_Ptr_Type(Point);
               end if;
               Match (Right_Paren);
            when Keyword_Uniform =>
               Advance;
               Match (Left_Paren);
               Uniform := new Uniform_Type;
               Expr (Uniform.Min);
               Match (Comma);
               Expr (Uniform.Max);
               Rtn := Node_Ptr_Type(Uniform);
               Match (Right_Paren);
            when Keyword_Normal =>
               Advance;
               Match (Left_Paren);
               Normal := new Normal_Type;
               Expr (Normal.Mu);
               Match (Comma);
               Expr (Normal.Sigma);
               Rtn := Node_Ptr_Type(Normal);
               Match (Right_Paren);
            when Keyword_Exponential =>
               Advance;
               Match (Left_Paren);
               Exponential := new Exponential_Type;
               Expr (Exponential.Beta);
               Rtn := Node_Ptr_Type(Exponential);
               Match (Right_Paren);
            when Id =>
               Id_Ref := new Id_Ref_Type;
               Id_Ref.Id := To_Handle(Token_String);
               Advance;
               if Look_Ahead = Left_Paren then
                  Advance;
                  loop
                     Expr (Actual);
                     Append_Node(Id_Ref.Actuals, Actual);
                     exit when Look_Ahead /= Comma;
                     Advance;
                  end loop;
                  Match (Right_Paren);
               end if;
               Rtn := Node_Ptr_Type(Id_Ref);
            when Number =>
               Literal := new Literal_Type;
               Literal.Value := Real'Value(Token_String);
               Rtn := Node_Ptr_Type(Literal);
               Advance;
            when others =>
               raise Syntax_Error with "expected an arithmetic factor";
         end case;
         Debug ("-Factor");
      end Factor;

   -- Parse the input string as a TL command.  Raise a Syntax_Error exception
   -- if parsing fails.  Scanner may also raise Bad_Character if it sees
   -- a character it does not know.  Return a syntax tree for the input
   -- string in Rtn_Tree.  If Debug_Mode is true, print debugging information
   -- about the progress of the parsing.
   begin
      Advance;
      Model(Rtn_Tree);
   end Parse;

end Parser;
