-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

-- <model> = { <description-section> <instance-section> } EOF
-- <description-section> = "description" { <named-description> }
-- <named-description-head> =  ID  [ ( <parameter-list> ) ] <named-description-tail>
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

with Ada.Text_IO, Ada.Characters.Handling, Ada.Strings.Fixed;
use Ada.Text_IO, Ada.Characters.Handling, Ada.Strings.Fixed;

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
         Scan_Next_Token (S, Start_Index, End_Index, Line_Number, Look_Ahead);
         Debug_Token (Start_Index, End_Index, Line_Number, Look_Ahead);
      end Advance;

      -- Make sure the next token in the lookahead matches one we
      -- specify.  If not, signal a syntax error.
      procedure Match (Token : in Token_T) is
      begin
         if Look_Ahead = Token then
            Advance;
         else
            raise Syntax_Error
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
               null;
            when End_Input =>
               null;
            when others =>
               null;
         end case;
         Debug ("-Model");
      end Model;

      -- <description-section> --> description <named-description>
      --                              { <named-description> }
      --                              
      -- First Set: {Keyword_Description}
      procedure Description_Section is
      begin
         Debug ("+Description_Section");
         
         
         case Look_Ahead is
            when Keyword_Description =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Description_Section");
      end Description_Section;

      -- 3. <named-description-head> --> ID [(<parameter-list>)] <named-description-tail>
      -- 
      -- First Set: {ID}
      procedure Named_Description_Head is
      begin
         Debug ("+Named_Description_Head");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Named_Description_Head");
      end Named_Description_Head;
      
      -- 3. <named-description> --> = <description> ; | : <description> ;
      -- 
      -- First Set: {Equals, Colon}
      procedure Named_Description_Tail is
      begin
         Debug ("+Named_Description_Tail");
         
         
         case Look_Ahead is
            when Equals =>
              null;
            when Colon =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Named_Description_Tail");
      end Named_Description_Tail;

      
      -- 4. <parameter-list> --> <parameter> {, <parameter> }
      -- 
      -- First Set: {ID}
      procedure Parameter_List is
      begin
         Debug ("+Parameter_List");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Parameter_List");
      end Parameter_List;

      -- 5. <parameter> --> ID : <type-id>
      -- 
      -- First Set: {ID}
      procedure Parameter is
      begin
         Debug ("+Parameter");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Parameter");
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
              null;
            when Keyword_Point =>
              null;
            when Keyword_Segment =>
               null;
            when Keyword_Route =>
               null;
            when Keyword_Friend =>
               null;
            when Keyword_Schedule =>
               null;
            when Keyword_Sensor =>
               null;
            when others =>
               null;
         end case;
           
      end Type_ID;

      -- 7. <description> --> <expr>
      --                    | <segment-description> | <point-description>
      --                    | <route-description> | <friend-description> |
      --                    | <trip-description>  | <threat-description>  |
      --                    | <schedule-description> | <sensor-description>
      --                    
      -- First Set: {Number, Left_Paren, Keyword_Segment,
      -- Keyword_Route, Keyword_Friend, Keyword_Schedule,
      -- Keyword_Sensor}
      procedure Description is
      begin
         Debug ("+Description");
         
         
         case Look_Ahead is
            when Number =>
              null;
            when Left_Paren =>
               null;
            when Keyword_Segment =>
               null;
            when Keyword_Route =>
               null;
            when Keyword_Friend =>
               null;
            when Keyword_Schedule =>
               null;
            when Keyword_Sensor =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Description");
      end Description;

      -- 9. <segment-description> --> segment ID -> ID [ <with-attributes> ]
      -- 
      -- First Set: {Keyword_Segment}
      procedure Segment_Description is
      begin
         Debug ("+Segment_Description");
         
         
         case Look_Ahead is
            when Keyword_Segment =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Segment_Description");
      end Segment_Description;

      -- 10. <route-description> --> route ( <segment-list> )
      -- 
      -- First Set: {Keyword_Route}
      procedure Route_Description is
      begin
         Debug ("+Route_Description");
         
         
         case Look_Ahead is
            when Keyword_Route =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Route_Description");
      end Route_Description;

      -- 11. <segment-list> --> <segment-id> {, <segment-id> }
      -- 
      -- First Set: {ID, Tilde}
      procedure Segment_List is
      begin
         Debug ("+Segment_List");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when Tilde =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Segment_List");
      end Segment_List;

      -- 12. <segment-id> --> ID | ~ ID
      -- 
      -- First Set: {ID, Tilde}
      procedure Segment_ID is
      begin
         Debug ("+Segment_Id");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when Tilde =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Segment_Id");
      end Segment_ID;

      -- 13. <friend-description> --> friend <expr> [ <with-attributes> ]
      -- 
      -- First Set: {Keyword_Friend}
      procedure Friend_Description is
      begin
         Debug ("+Friend_Description");
         
         
         case Look_Ahead is
            when Keyword_Friend =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Friend_Description");
      end Friend_Description;

      -- 14. <trip-description> --> trip ID -> ID [ <with-attributes> ]
      -- 
      -- First Set: {Keyword_Trip}
      procedure Trip_Description is
      begin
         Debug ("+Trip_Description");
         
         
         case Look_Ahead is
            when Keyword_Trip =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Trip_Description");
      end Trip_Description;

      -- 15. <threat-description> --> threat (<id-list>) [<with-attributes>]
      -- 
      -- First Set: {Keyword_Threat}
      procedure Threat_Description is
      begin
         Debug ("+Threat_Description");
         
         
         case Look_Ahead is
            when Keyword_Threat =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Threat_Description");
      end Threat_Description;

      -- 16. <schedule-description> --> schedule  [ <with-attributes> ]
      -- 
      -- First Set: {Keyword_Schedule}
      procedure Schedule_Description is
      begin
         Debug ("+Schedule_Description");
         
         
         case Look_Ahead is
            when Keyword_Schedule =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Schedule_Description");
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
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Sensor_Description");
      end Sensor_Description;

      -- 18. <with-attributes> --> with <attribute-list>
      -- 
      -- First Set: {Keyword_With}
      procedure With_Attributes is
      begin
         Debug ("+With_Attributes");
         
         
         case Look_Ahead is
            when Keyword_With =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-With_Attributes");
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
            when Keyword_Trafficability =>
              null;
            when Keyword_Vulnerability =>
              null;
            when Keyword_Range =>
               null;
            when Keyword_Effectiveness =>
               null;
            when Keyword_Schedule =>
               null;
            when Keyword_Start =>
               null;
            when Keyword_Interval =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Attribute_List");
      end Attribute_List;

      -- 20. <attribute-pair> --> <expr-attribute-name> = <expr>
      -- 
      -- First Set: {Keyword_Trafficability, Keyword_Vulnerability,
      -- Keyword_Range, Keyword_Effectiveness, Keyword_Schedule,
      -- Keyword_Start, Keyword_Interval}
      procedure Attribute_Pair is
      begin
         Debug ("+Attribute_Pair");
         
         
         case Look_Ahead is
            when Keyword_Trafficability =>
              null;
            when Keyword_Vulnerability =>
              null;
            when Keyword_Range =>
               null;
            when Keyword_Effectiveness =>
               null;
            when Keyword_Schedule =>
               null;
            when Keyword_Start =>
               null;
            when Keyword_Interval =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Attribute_Pair");
      end Attribute_Pair;
      
      -- 21. <id-list> = ID { "," ID }
      -- 
      -- First Set: {ID}
      procedure ID_List is
      begin
         Debug ("+ID_List");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-ID_List");
      end ID_List;
      
      -- 22. <instance-section> --> instance <instance> { <instance> }
      -- 
      -- First Set: {Keyword_Instance}
      procedure Instance_Section is
      begin
         Debug ("+Instance_Section");
         
         
         case Look_Ahead is
            when Keyword_Instance =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Instance_Section");
      end Instance_Section;

      -- 23. <instance> --> ID : ID  [ ( <expr-list>  ) ] ;
      -- 
      -- First Set: {ID}
      procedure Instance is
      begin
         Debug ("+Instance");
         
         
         case Look_Ahead is
            when ID =>
              null;
            when others =>
               null;
         end case;
           
         Debug ("-Instance");
      end Instance;
      
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Expr_List is
      begin
         Debug ("+Expr_List");
         
         
         case Look_Ahead is
            when Minus =>
              null;
            when Left_Paren =>
               null;
            when ID =>
               null;
            when Number =>
               null;
            when Keyword_Uniform =>
               null;
            when Keyword_Exponential =>
               null;
            when Keyword_Normal =>
               null;
            when others =>
               null;
         end case;
           
           
         Debug ("-Expr_List");
      end Expr_List;
        
      -- 25. <expr-attribute-name> --> trafficability | vulnerability | range
      --                        | effectiveness | schedule | start | interval
      --                        
      -- First Set: {Keyword_Trafficability, Keyword_Vulnerability,
      -- Keyword_Range, Keyword_Effectiveness, Keyword_Schedule,
      -- Keyword_Start, Keyword_Interval}
      procedure Expr_Attribute_Name is
      begin
         Debug ("+Expr_Attribute_Name");
         
         
         case Look_Ahead is
            when Keyword_Trafficability =>
              null;
            when Keyword_Vulnerability =>
              null;
            when Keyword_Range =>
               null;
            when Keyword_Effectiveness =>
               null;
            when Keyword_Schedule =>
               null;
            when Keyword_Start =>
               null;
            when Keyword_Interval =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Expr_Attribute_Name");
      end Expr_Attribute_Name;

      -- 26. <expr> --> <term> { <addop> <term> }
      -- 
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Expr is
      begin
         Debug ("+Expr");
         
         
         case Look_Ahead is
            when Minus =>
              null;
            when Left_Paren =>
               null;
            when ID =>
               null;
            when Number =>
               null;
            when Keyword_Uniform =>
               null;
            when Keyword_Exponential =>
               null;
            when Keyword_Normal =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Expr");
      end Expr;

      -- 27. <term> --> <signed-factor> { <mulop> <signed-factor> }
      -- 
      -- First Set: {Minus, Left_Paren, ID, Number, Keyword_Uniform,
      -- Keyword_Exponential, Keyword_Normal}
      procedure Term is
      begin
         Debug ("+Term");
         
         
         case Look_Ahead is
            when Minus =>
              null;
            when Left_Paren =>
               null;
            when ID =>
               null;
            when Number =>
               null;
            when Keyword_Uniform =>
               null;
            when Keyword_Exponential =>
               null;
            when Keyword_Normal =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Term");
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
              null;
            when Left_Paren =>
               null;
            when ID =>
               null;
            when Number =>
               null;
            when Keyword_Uniform =>
               null;
            when Keyword_Exponential =>
               null;
            when Keyword_Normal =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Signed_Factor");
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
              null;
            when ID =>
              null;
            when Number =>
               null;
            when Keyword_Uniform =>
               null;
            when Keyword_Exponential =>
               null;
            when Keyword_Normal =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Factor");
      end Factor;
      
      -- First Set: {Keyword_Uniform, Keyword_Exponential, Keyword_Normal}
      procedure Random_Var is
      begin
         Debug ("+Random_Var");
         
         
         case Look_Ahead is
            when Keyword_Uniform =>
              null;
            when Keyword_Exponential =>
              null;
            when Keyword_Normal =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Random_Var");
      end Random_Var;
      
      -- First Set: {Plus, Minus}
      procedure Add_Op is
      begin
         Debug ("+Add_Op");
         
         
         case Look_Ahead is
            when Plus =>
              null;
            when Minus =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Add_Op");
      end Add_Op;
      
      -- First Set: {Star, Slash}
      procedure Mul_Op is
      begin
         Debug ("+Mul_Op");
         
         
         case Look_Ahead is
            when Star =>
              null;
            when Slash =>
               null;
            when others =>
               null;
         end case;
           
         Debug ("-Mul_Op");
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
