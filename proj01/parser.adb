-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

-- <model> = { <description-section> <instance-section> } EOF
-- <description-section> = "description" { <named-description> }
-- <named-description-head> =  ID  [ ( <parameter-list> ) ]
--                             <named-description-tail>
-- <named-description-tail> = "=" <description> ";" | ":" <type-id> ";"
-- <parameter-list> = <parameter> {, <parameter> }
-- <parameter> = ID ":" <type-id>
-- <type-id> = "number" | "point" | "segment" | "route" | "friend" | "schedule"
--        | "sensor"
-- <description> = <expr> | <segment-description>
--                | <route-description> | <friend-description> |
--                | <trip-description>  | <threat-description>  |
--                | <schedule-description> | <sensor-description>
-- <segment-description> = "segment" ID -> ID [ <with-attributes> ]
-- <route-description> = "route" ( <segment-list> )
-- <segment-list> = <segment-id> {, <segment-id> }
-- <segment-id> = ID | "~" ID
-- <friend-description> = "friend" <expr> [ <with-attributes> ]
-- <trip-description> = "trip" ID "->" ID [ <with-attributes> ]
-- <threat-description> = "threat" ( <id-list>  ) [ <with-attributes> ]
-- <schedule-description> = "schedule"  [ <with-attributes> ]
-- <sensor-description> = "sensor" <expr> ->
--                            ( <id-list>  ) [ <with-attributes> ]
-- <with-attributes> = "with" <attribute-list>
-- <attribute-list> = <attribute-pair> {, <attribute-pair> }
-- <attribute-pair> = <expr-attribute-name> = <expr>
-- <id-list> = ID { "," ID }
-- <instance-section> = "instance" { <instance> }
-- <instance> = ID ":" ID  [ ( <expr-list>  ) ] ;
-- <expr-list> = <expr> { , <expr> }
-- <expr-attribute-name> = "trafficability" | "vulnerability" | "range"
--                   | "effectiveness" | "schedule" | "start" | "interval"
-- <expr> = <term> { <addop> <term> }
-- <term> = <signed-factor> { <mulop> <signed-factor> }
-- <signed-factor> = - <factor> | <factor>
-- <factor> = "(" <expr> ")"
--           | "(" <expr> "," <expr> ")"
--           | <random-var>
--           | ID [ "(" <expr-list>  ")" ]
--           | NUMBER
-- <random-var> = uniform ( <expr> , <expr> )
--               | normal ( <expr> , <expr> )
--               | exponential ( <expr> )
-- <addop> = "+" | "-"
-- <mulop> = "*" | "/"

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Ada.Containers.Doubly_Linked_Lists;

with Scanner; use Scanner;

package body Parser is
   
   procedure Parse
     (S          : in String;
      Rtn_Tree   : out Node_A;
      Debug_Mode : in Boolean := False)
   is

      -- A debugging print routine with indentation control.  If first
      -- character of Msg is '+', indentation is increased by one
      -- level after printing the message.  If it's '-', indentation
      -- is decreased by one level before printing.
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
                            Token : in Token_T) is
      begin
         if Debug_Mode then
            Put_Scanned_Token(S, Start_Index, End_Index, Line_Number,
                              Token, Indentation);
            New_Line;
         end if;
      end Debug_Token;

      -- These describe the current lookahead token.
      Start_Index : Positive   := 1;  -- Start of token string.
      End_Index   : Natural    := 0;  -- End of token string.
      Line_Number : Positive   := 1;
      Look_Ahead  : Token_T := End_Input; -- Enumerated type of token.

      -- Return the current lookahead token string by slicing the
      -- input.
      function Token_String return String is
      begin
         return S (Start_Index .. End_Index);
      end Token_String;

      -- Scan the next token into the lookahead and print a
      -- description if in debug mode.
      procedure Advance is
      begin
         Debug_Token (Start_Index, End_Index, Line_Number, Look_Ahead);
         Scan_Next_Token (S, Start_Index, End_Index, Line_Number, Look_Ahead);
      end Advance;

      Unexpected_Token_Error : exception;

      -- Make sure the next token in the lookahead matches one we
      -- specify.  If not, signal a syntax error.
      procedure Match (Token : in Token_T) is
      begin
         if Look_Ahead = Token then
            Advance;
         else
            raise Unexpected_Token_Error
              with "expected a " & To_Lower (Token_T'Image (Token));
         end if;
      end Match;

      procedure Model;
      procedure Description_Section;
      procedure Named_Description_Head;
      procedure Named_Description_Tail;
      procedure Parameter_List;
      procedure Parameter;
      procedure Type_ID;
      procedure Description;
      procedure Segment_Description;
      procedure Route_Description;
      procedure Segment_List;
      procedure Segment_ID;
      procedure Friend_Description;
      procedure Trip_Description;
      procedure Threat_Description;
      procedure Schedule_Description;
      procedure Sensor_Description;
      procedure With_Attributes;
      procedure Attribute_List;
      procedure Attribute_Pair;
      procedure ID_List;
      procedure Instance_Section;
      procedure Instance;
      procedure Expr_List;
      procedure Expr_Attribute_Name;
      procedure Expr;
      procedure Term;
      procedure Signed_Factor;
      procedure Factor;
      procedure Random_Var;
      procedure Add_Op;
      procedure Mul_Op;

      -- 1. <model> --> { <description-section> <instance-section> } EOF
      --
      -- First Set: {Keyword_Description, End_Input}
      procedure Model is
      begin
         Debug ("+Model");
         case Look_Ahead is
            when Keyword_Description =>
               Description_Section;
            when End_Input =>
               null;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Model";
         end case;
         Debug ("-Model");

      exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err) & " in Model";
      end Model;

      -- <description-section> --> description <named-description-head>
      --                              { <named-description-head> }
      --
      -- First Set: {Keyword_Description}
      procedure Description_Section is
      begin
         Debug ("+Description_Section");

         case Look_Ahead is
            when Keyword_Description =>
               Match(Keyword_Description);
               Named_Description_Head;
               while Look_Ahead = ID loop
                  Named_Description_Head;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Description_Section";
         end case;

         Debug ("-Description_Section");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Description_Section";
      end Description_Section;

      -- 3. <named-description-head> --> ID [(<parameter-list>)]
      --                                 <named-description-tail>
      --
      -- First Set: {ID}
      procedure Named_Description_Head is
      begin
         Debug ("+Named_Description_Head");

         case Look_Ahead is
            when ID =>
               Match(ID);
               if Look_Ahead = Left_Paren then
                  Match(Left_Paren);
                  Parameter_List;
                  Match(Right_Paren);
               end if;
               Named_Description_Tail;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Named_Description_Head";
         end case;

         Debug ("-Named_Description_Head");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Named_Description_Head";
      end Named_Description_Head;

      -- 3. <named-description> --> = <description> ; | : <type-id> ;
      --
      -- First Set: {Equals, Colon}
      procedure Named_Description_Tail is
      begin
         Debug ("+Named_Description_Tail");

         case Look_Ahead is
            when Equals =>
               Match(Equals);
               Description;
               Match(Semi);
            when Colon =>
               Match(Colon);
               Type_ID;
               Match(Semi);
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Named_Description_Tail";
         end case;

         Debug ("-Named_Description_Tail");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Named_Description_Tail";
      end Named_Description_Tail;


      -- 4. <parameter-list> --> <parameter> {, <parameter> }
      --
      -- First Set: {ID}
      procedure Parameter_List is
      begin
         Debug ("+Parameter_List");

         case Look_Ahead is
            when ID =>
               Parameter;
               while Look_Ahead = ID loop
                  Match(Comma);
                  Parameter;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Parameter_List";
         end case;

         Debug ("-Parameter_List");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Parameter_List";
      end Parameter_List;

      -- 5. <parameter> --> ID : <type-id>
      --
      -- First Set: {ID}
      procedure Parameter is
      begin
         Debug ("+Parameter");

         case Look_Ahead is
            when ID =>
               Match(ID);
               Match(Colon);
               Type_ID;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Parameter";
         end case;

         Debug ("-Parameter");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Parameter";
      end Parameter;

      -- 6. <type-id> --> number | point | segment | route | friend | schedule
      --            | sensor
      --
      -- First Set: {Keyword_Number, Keyword_Point, Keyword_Segment,
      -- Keyword_Route, Keyword_Friend, Keyword_Schedule,
      -- Keyword_Sensor}
      procedure Type_ID is
      begin
         Debug ("+Type_Id");

         case Look_Ahead is
            when Keyword_Number =>
               Advance;
            when Keyword_Point =>
               Advance;
            when Keyword_Segment =>
               Advance;
            when Keyword_Route =>
               Advance;
            when Keyword_Friend =>
               Advance;
            when Keyword_Schedule =>
               Advance;
            when Keyword_Sensor =>
               Advance;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Type_ID";
         end case;
         Debug ("-Type_Id");
      end Type_ID;

      -- 7. <description> --> <expr>
      --                    | <segment-description>
      --                    | <route-description> | <friend-description> |
      --                    | <trip-description>  | <threat-description>  |
      --                    | <schedule-description> | <sensor-description>
      --
      -- First Set: {Number, Left_Paren, Keyword_Segment,
      -- Keyword_Route, Keyword_Friend, Keyword_Schedule,
      -- Keyword_Sensor, Keyword_Threat, Keyword_Trip}
      procedure Description is
      begin
         Debug ("+Description");

         case Look_Ahead is
            when Number | Left_Paren =>
               Expr;
            when Keyword_Segment =>
               Segment_Description;
            when Keyword_Route =>
               Route_Description;
            when Keyword_Friend =>
               Friend_Description;
            when Keyword_Schedule =>
               Schedule_Description;
            when Keyword_Sensor =>
               Sensor_Description;
            when Keyword_Trip =>
               Trip_Description;
            when Keyword_Threat =>
               Threat_Description;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Description";
         end case;

         Debug ("-Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Type_ID";
      end Description;

      -- 9. <segment-description> --> segment ID -> ID [ <with-attributes> ]
      --
      -- First Set: {Keyword_Segment}
      procedure Segment_Description is
      begin
         Debug ("+Segment_Description");

         case Look_Ahead is
            when Keyword_Segment =>
               Match(Keyword_Segment);
               Match(ID);
               Match(Arrow);
               Match(ID);
               if Look_Ahead = Keyword_With then
                  With_Attributes;
               end if;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Segment_Description";
         end case;

         Debug ("-Segment_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Segment_Description";
      end Segment_Description;

      -- 10. <route-description> --> route ( <segment-list> )
      --
      -- First Set: {Keyword_Route}
      procedure Route_Description is
      begin
         Debug ("+Route_Description");

         case Look_Ahead is
            when Keyword_Route =>
               Match(Keyword_Route);
               Match(Left_Paren);
               Segment_List;
               Match(Right_Paren);
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Route_Description";
         end case;

         Debug ("-Route_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Route_Description";
      end Route_Description;

      -- 11. <segment-list> --> <segment-id> {, <segment-id> }
      --
      -- First Set: {ID, Tilde}
      procedure Segment_List is
      begin
         Debug ("+Segment_List");

         case Look_Ahead is
            when ID | Tilde =>
               Segment_ID;
               while Look_Ahead = Comma loop
                  Match(Comma);
                  Segment_ID;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Segment_List";
         end case;

         Debug ("-Segment_List");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Segment_List";
      end Segment_List;

      -- 12. <segment-id> --> ID | ~ ID
      --
      -- First Set: {ID, Tilde}
      procedure Segment_ID is
      begin
         Debug ("+Segment_Id");

         case Look_Ahead is
            when ID =>
               Advance;
            when Tilde =>
               Match(Tilde);
               Match(ID);
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Segment_ID";
         end case;

         Debug ("-Segment_Id");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Segment_ID";
      end Segment_ID;

      -- 13. <friend-description> --> friend <expr> [ <with-attributes> ]
      --
      -- First Set: {Keyword_Friend}
      procedure Friend_Description is
      begin
         Debug ("+Friend_Description");

         case Look_Ahead is
            when Keyword_Friend =>
               Match(Keyword_Friend);
               Expr;
               if Look_Ahead = Keyword_With then
                  With_Attributes;
               end if;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Friend_Description";
         end case;

         Debug ("-Friend_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Friend_Description";
      end Friend_Description;

      -- 14. <trip-description> --> trip ID -> ID [ <with-attributes> ]
      --
      -- First Set: {Keyword_Trip}
      procedure Trip_Description is
      begin
         Debug ("+Trip_Description");

         case Look_Ahead is
            when Keyword_Trip =>
               Match(Keyword_Trip);
               Match(ID);
               Match(Arrow);
               Match(ID);
               if Look_Ahead = Keyword_With then
                  With_Attributes;
               end if;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Trip_Description";
         end case;

         Debug ("-Trip_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Trip_Description";
      end Trip_Description;

      -- 15. <threat-description> --> threat (<id-list>) [<with-attributes>]
      --
      -- First Set: {Keyword_Threat}
      procedure Threat_Description is
      begin
         Debug ("+Threat_Description");

         case Look_Ahead is
            when Keyword_Threat =>
               Match(Keyword_Threat);
               Match(Left_Paren);
               ID_List;
               Match(Right_Paren);
               if Look_Ahead = Keyword_With then
                  With_Attributes;
               end if;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Threat_Description";
         end case;

         Debug ("-Threat_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Threat_Description";
      end Threat_Description;

      -- 16. <schedule-description> --> schedule  [ <with-attributes> ]
      --
      -- First Set: {Keyword_Schedule}
      procedure Schedule_Description is
      begin
         Debug ("+Schedule_Description");

         case Look_Ahead is
            when Keyword_Schedule =>
               Match(Keyword_Schedule);
               if Look_Ahead = Keyword_With then
                  With_Attributes;
               end if;

            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Schedule_Description";
         end case;

         Debug ("-Schedule_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Schedule_Description";
      end Schedule_Description;

      -- 17. <sensor-description> --> sensor <expr> ->
      --                                 ( <id-list>  ) [ <with-attributes> ]
      --
      -- First Set: {Keyword_Sensor}
      procedure Sensor_Description is
      begin
         Debug ("+Sensor_Description");

         case Look_Ahead is
            when Keyword_Sensor =>
               Match(Keyword_Sensor);
               Expr;
               Match(Arrow);
               Match(Left_Paren);
               ID_List;
               Match(Right_Paren);
               if Look_Ahead = Keyword_With then
                  With_Attributes;
               end if;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Sensor_Description";
         end case;

         Debug ("-Sensor_Description");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Sensor_Description";
      end Sensor_Description;

      -- 18. <with-attributes> --> with <attribute-list>
      --
      -- First Set: {Keyword_With}
      procedure With_Attributes is
      begin
         Debug ("+With_Attributes");

         case Look_Ahead is
            when Keyword_With =>
               Match(Keyword_With);
               Attribute_List;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in With_Attributes";
         end case;

         Debug ("-With_Attributes");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in With_Attributes";
      end With_Attributes;

      -- 19. <attribute-list> --> <attribute-pair> {, <attribute-pair> }
      --
      -- First Set: {Keyword_Trafficability, Keyword_Vulnerability,
      -- Keyword_Range, Keyword_Effectiveness, Keyword_Schedule,
      -- Keyword_Start, Keyword_Interval}
      procedure Attribute_List is
      begin
         Debug ("+Attribute_List");

         case Look_Ahead is
            when Keyword_Trafficability | Keyword_Vulnerability | Keyword_Range
              | Keyword_Effectiveness | Keyword_Sensor |Keyword_Schedule
              | Keyword_Start | Keyword_Interval =>
               Attribute_Pair;
               while Look_Ahead = Comma loop
                  Match(Comma);
                  Attribute_Pair;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Attribute_List";
         end case;

         Debug ("-Attribute_List");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Attribute_List";
      end Attribute_List;

      -- 20. <attribute-pair> --> <expr-attribute-name> = <expr>
      --
      -- First Set: {Keyword_Trafficability, Keyword_Vulnerability,
      -- Keyword_Range, Keyword_Effectiveness, Keyword_Sensor,
      -- Keyword_Schedule, Keyword_Start, Keyword_Interval}
      procedure Attribute_Pair is
      begin
         Debug ("+Attribute_Pair");

         case Look_Ahead is
            when Keyword_Trafficability | Keyword_Vulnerability | Keyword_Range
              | Keyword_Effectiveness | Keyword_Sensor | Keyword_Schedule
              | Keyword_Start | Keyword_Interval =>
               Expr_Attribute_Name;
               Match(Equals);
               Expr;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Attribute_Pair";
         end case;

         Debug ("-Attribute_Pair");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Attribute_Pair";
      end Attribute_Pair;

      -- 21. <id-list> = ID { "," ID }
      --
      -- First Set: {ID}
      procedure ID_List is
      begin
         Debug ("+ID_List");

         case Look_Ahead is
            when ID =>
               Match(ID);
               while Look_Ahead = Comma loop
                  Match(Comma);
                  Match(ID);
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in ID_List";
         end case;

         Debug ("-ID_List");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in ID_List";
      end ID_List;

      -- 22. <instance-section> --> instance <instance> { <instance> }
      --
      -- First Set: {Keyword_Instance}
      procedure Instance_Section is
      begin
         Debug ("+Instance_Section");

         case Look_Ahead is
            when Keyword_Instance =>
               Match(Keyword_Instance);
               Instance;
               while Look_Ahead = ID loop
                  Instance;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Instance_Section";
         end case;

         Debug ("-Instance_Section");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Instance_Section";
      end Instance_Section;

      -- 23. <instance> --> ID : ID  [ ( <expr-list>  ) ] ;
      --
      -- First Set: {ID}
      procedure Instance is
      begin
         Debug ("+Instance");

         case Look_Ahead is
            when ID =>
               Match(ID);
               Match(Colon);
               Match(ID);
               if Look_Ahead = Left_Paren then
                  Match(Left_Paren);
                  Expr_List;
                  Match(Right_Paren);
               end if;
               Match(Semi);
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Instance";
         end case;

         Debug ("-Instance");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Instance";
      end Instance;

      -- <expr-list> = <expr> { , <expr> }
      --
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Expr_List is
      begin
         Debug ("+Expr_List");

         case Look_Ahead is
            when Minus | Left_Paren | ID | Number | Keyword_Uniform
              | Keyword_Exponential | Keyword_Normal =>
               Expr;
               if Look_Ahead = Comma then
                  Match(Comma);
                  Expr_List;
               end if;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Expr_List";
         end case;

         Debug ("-Expr_List");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Expr_List";
      end Expr_List;

      -- 25. <expr-attribute-name> --> trafficability | vulnerability | range
      --                        | effectiveness | schedule | start | interval
      --
      -- First Set: {Keyword_Trafficability, Keyword_Vulnerability,
      --  Keyword_Range, Keyword_Effectiveness, Keyword_Schedule,
      --  Keyword_Sensor, Keyword_Start, Keyword_Interval}
      procedure Expr_Attribute_Name is
      begin
         Debug ("+Expr_Attribute_Name");

         case Look_Ahead is
            when Keyword_Trafficability =>
               Advance;
            when Keyword_Vulnerability =>
               Advance;
            when Keyword_Range =>
               Advance;
            when Keyword_Effectiveness =>
               Advance;
            when Keyword_Sensor =>
               Advance;
            when Keyword_Schedule =>
               Advance;
            when Keyword_Start =>
               Advance;
            when Keyword_Interval =>
               Advance;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Expr_Attribute_Name";
         end case;

         Debug ("-Expr_Attribute_Name");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Expr_Attribute_Name";
      end Expr_Attribute_Name;

      -- 26. <expr> --> <term> { <addop> <term> }
      --
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Expr is
      begin
         Debug ("+Expr");

         case Look_Ahead is
            when Minus | Left_Paren | ID | Number | Keyword_Uniform |
              Keyword_Exponential | Keyword_Normal =>
               Term;
               while Look_Ahead = Plus or else Look_Ahead = Minus loop
                  Add_Op;
                  Term;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Expr";
         end case;

         Debug ("-Expr");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Expr";
      end Expr;

      -- 27. <term> --> <signed-factor> { <mulop> <signed-factor> }
      --
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Term is
      begin
         Debug ("+Term");

         case Look_Ahead is
            when Minus | Left_Paren | ID | Number | Keyword_Uniform |
              Keyword_Exponential | Keyword_Normal =>
               Signed_Factor;
               while Look_Ahead = Star or else Look_Ahead = Slash loop
                  Mul_Op;
                  Signed_Factor;
               end loop;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Term";
         end case;

         Debug ("-Term");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Term";
      end Term;

      -- 28. <signed-factor> --> - <factor> | <factor>
      --
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Signed_Factor is
      begin
         Debug ("+Signed_Factor");

         case Look_Ahead is
            when Minus =>
               Match(Minus);
               Factor;
            when Left_Paren | ID | Number | Keyword_Uniform |
              Keyword_Exponential | Keyword_Normal =>
               Factor;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Signed_Factor";
         end case;

         Debug ("-Signed_Factor");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Signed_Factor";
      end Signed_Factor;

      -- 29. <factor> --> ( <expr> [, <expr>] )
      --                | <random-var>
      --                | ID [ ( <expr-list>  ) ]
      --                | NUMBER
      --
      -- First Set: {Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Factor is
      begin
         Debug ("+Factor");

         case Look_Ahead is
            when Left_Paren =>
               Match(Left_Paren);
               Expr;
               if Look_Ahead = Comma then
                  Match(Comma);
                  Expr;
               end if;
               Match(Right_Paren);
            when ID =>
               Match(ID);
               if Look_Ahead = Left_Paren then
                  Match(Left_Paren);
                  Expr_List;
                  Match(Right_Paren);
               end if;
            when Number =>
               Advance;
            when Keyword_Uniform | Keyword_Exponential | Keyword_Normal =>
               Random_Var;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Factor";
         end case;

         Debug ("-Factor");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Factor";
      end Factor;
      -- <random-var> = uniform ( <expr> , <expr> )
      --               | normal ( <expr> , <expr> )
      --               | exponential ( <expr> )
      --
      -- First Set: {Keyword_Uniform, Keyword_Exponential, Keyword_Normal}
      procedure Random_Var is
      begin
         Debug ("+Random_Var");

         case Look_Ahead is
            when Keyword_Uniform =>
               Match(Keyword_Uniform);
               Match(Left_Paren);
               Expr;
               Match(Comma);
               Expr;
               Match(Right_Paren);
            when Keyword_Exponential =>
               Match(Keyword_Exponential);
               Match(Left_Paren);
               Expr;
               Match(Right_Paren);
            when Keyword_Normal =>
               Match(Keyword_Normal);
               Match(Left_Paren);
               Expr;
               Match(Comma);
               Expr;
               Match(Right_Paren);
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Random_Var";
         end case;

         Debug ("-Random_Var");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Random_Var";
      end Random_Var;

      -- First Set: {Plus, Minus}
      procedure Add_Op is
      begin
         Debug ("+Add_Op");

         case Look_Ahead is
            when Plus =>
               Advance;
            when Minus =>
               Advance;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Add_Op";
         end case;

         Debug ("-Add_Op");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Add_Op";
      end Add_Op;

      -- First Set: {Star, Slash}
      procedure Mul_Op is
      begin
         Debug ("+Mul_Op");

         case Look_Ahead is
            when Star =>
               Advance;
            when Slash =>
               Advance;
            when others =>
               raise Syntax_Error with "Unexpected token "
                 & Token_T'Image(Look_Ahead) & " in Mul_Op";
         end case;

         Debug ("-Mul_Op");

         exception
         when Err : Unexpected_Token_Error =>
            raise Syntax_Error with Exception_Message(Err)
              & " in Mul_Op";
      end Mul_Op;

   -- Parse the input string as a TL command.  Raise a Syntax_Error exception
   -- if parsing fails.  Scanner may also raise Bad_Character if it sees
   -- a character it does not know.  Return a syntax tree for the input
   -- string in Rtn_Tree.  If Debug_Mode is true, print debugging information
   -- about the progress of the parsing.
   begin
      Advance;
      Model;
      Rtn_Tree := null;
   end Parse;

end Parser;
