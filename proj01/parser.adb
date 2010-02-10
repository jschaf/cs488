-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

-- <model> = { <description-section> <instance-section> } EOF
-- <description-section> = "description" { <named-description> }
-- <named-description-head> =  ID  [ ( <parameter-list> ) ] <named-description-tail>
-- <named-description-tail> = "=" <description> ";" | ":" <type-id> ";"
-- <parameter-list> = <parameter> {, <parameter> }
-- <parameter> = ID ":" <type-id>
-- <type-id> = "number" | "point" | "segment" | "route" | "friend" | "schedule"
--         | "sensor"
-- <description> = <expr> | <segment-description>
--                 | <route-description> | <friend-description> |
--                 | <trip-description>  | <threat-description>  |
--                 | <schedule-description> | <sensor-description>
-- <segment-description> = "segment" ID -> ID [ <with-attributes> ]
-- <route-description> = "route" ( <segment-list> )
-- <segment-list> = <segment-id> {, <segment-id> }
-- <segment-id> = ID | "~" ID
-- <friend-description> = "friend" <expr> [ <with-attributes> ]
-- <trip-description> = "trip" ID "->" ID [ <with-attributes> ]
-- <threat-description> = "threat" ( <id-list>  ) [ <with-attributes> ]
-- <schedule-description> = "schedule"  [ <with-attributes> ]
-- <sensor-description> = "sensor" <expr> ->
--                             ( <id-list>  ) [ <with-attributes> ]
-- <with-attributes> = "with" <attribute-list>
-- <attribute-list> = <attribute-pair> {, <attribute-pair> }
-- <attribute-pair> = <expr-attribute-name> = <expr>
-- <id-list> = ID { "," ID }
-- <instance-section> = "instance" { <instance> }
-- <instance> = ID ":" ID  [ ( <expr-list>  ) ] ;
-- <expr-list> = <expr> { , <expr> }
-- <expr-attribute-name> = "trafficability" | "vulnerability" | "range"
--                    | "effectiveness" | "schedule" | "start" | "interval"
-- <expr> = <term> { <addop> <term> }
-- <term> = <signed-factor> { <mulop> <signed-factor> }
-- <signed-factor> = - <factor> | <factor>
-- <factor> = "(" <expr> ")"
--            | "(" <expr> "," <expr> ")"
--            | <random-var>
--            | ID [ "(" <expr-list>  ")" ]
--            | NUMBER
-- <random-var> = uniform ( <expr> , <expr> )
--                | normal ( <expr> , <expr> )
--                | exponential ( <expr> )
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
         Debug_Token (Start_Index, End_Index, Line_Number, Look_Ahead);
      end Advance;

      -- Make sure the next token in the lookahead matches one we specify.  If
      -- not, signal a syntax error.
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
      procedure Point_Description;
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

      --  1. <model> --> { <description-section> <instance-section> } EOF
      procedure Model is
      begin
         Debug ("+Model");
         -- to be completed!
         Debug ("-Model");
      end Model;

      --  2. <description-section> --> description <named-description>
      --                                  { <named-description> }
      procedure Description_Section is
      begin
         Debug ("+Description_Section");
         -- to be completed!
         Debug ("-Description_Section");
      end Description_Section;

      --  3. <named-description-head> --> ID [(<parameter-list>)] <named-description-tail>
      procedure Named_Description_Head is
      begin
         Debug ("+Named_Description_Head");
         -- to be completed!
         Debug ("-Named_Description_Head");
      end Named_Description_Head;
      
      --  3. <named-description> --> = <description> ; | : <description> ;
      procedure Named_Description_Tail is
      begin
         Debug ("+Named_Description_Tail");
         -- to be completed!
         Debug ("-Named_Description_Tail");
      end Named_Description_Tail;

      
      --  4. <parameter-list> --> <parameter> {, <parameter> }
      procedure Parameter_List is
      begin
         Debug ("+Parameter_List");
         -- to be completed!
         Debug ("-Parameter_List");
      end Parameter_List;

      --  5. <parameter> --> ID : <type-id>
      procedure Parameter is
      begin
         Debug ("+Parameter");
         -- to be completed!
         Debug ("-Parameter");
      end Parameter;

      --  6. <type-id> --> number | point | segment | route | friend | schedule
      --             | sensor
      procedure Type_ID is
      begin
         Debug ("+Type_Id");
         -- to be completed!
         Debug ("-Type_Id");
      end Type_ID;

      --  7. <description> --> <expr>
      --                     | <segment-description> | <point-description>
      --                     | <route-description> | <friend-description> |
      --                     | <trip-description>  | <threat-description>  |
      --                     | <schedule-description> | <sensor-description>
      procedure Description is
      begin
         Debug ("+Description");
         -- to be completed!
         Debug ("-Description");
      end Description;

      --  8. <point-description> -->  ( <expr> , <expr> )
      procedure Point_Description is
      begin
         Debug ("+Point_Description");
         -- to be completed!
         Debug ("-Point_Description");
      end Point_Description;

      --  9. <segment-description> --> segment ID -> ID [ <with-attributes> ]
      procedure Segment_Description is
      begin
         Debug ("+Segment_Description");
         -- to be completed!
         Debug ("-Segment_Description");
      end Segment_Description;

      --  10. <route-description> --> route ( <segment-list> )
      procedure Route_Description is
      begin
         Debug ("+Route_Description");
         -- to be completed!
         Debug ("-Route_Description");
      end Route_Description;

      --  11. <segment-list> --> <segment-id> {, <segment-id> }
      procedure Segment_List is
      begin
         Debug ("+Segment_List");
         -- to be completed!
         Debug ("-Segment_List");
      end Segment_List;

      --  12. <segment-id> --> ID | ~ ID
      procedure Segment_ID is
      begin
         Debug ("+Segment_Id");
         -- to be completed!
         Debug ("-Segment_Id");
      end Segment_ID;

      --  13. <friend-description> --> friend <expr> [ <with-attributes> ]
      procedure Friend_Description is
      begin
         Debug ("+Friend_Description");
         -- to be completed!
         Debug ("-Friend_Description");
      end Friend_Description;

      --  14. <trip-description> --> trip ID -> ID [ <with-attributes> ]
      procedure Trip_Description is
      begin
         Debug ("+Trip_Description");
         -- to be completed!
         Debug ("-Trip_Description");
      end Trip_Description;

      --  15. <threat-description> --> threat (<id-list>) [<with-attributes>]
      procedure Threat_Description is
      begin
         Debug ("+Threat_Description");
         -- to be completed!
         Debug ("-Threat_Description");
      end Threat_Description;

      --  16. <schedule-description> --> schedule  [ <with-attributes> ]
      procedure Schedule_Description is
      begin
         Debug ("+Schedule_Description");
         -- to be completed!
         Debug ("-Schedule_Description");
      end Schedule_Description;

      --  17. <sensor-description> --> sensor <expr> ->
      --                                  ( <id-list>  ) [ <with-attributes> ]
      procedure Sensor_Description is
      begin
         Debug ("+Sensor_Description");
         -- to be completed!
         Debug ("-Sensor_Description");
      end Sensor_Description;

      --  18. <with-attributes> --> with <attribute-list>
      procedure With_Attributes is
      begin
         Debug ("+With_Attributes");
         -- to be completed!
         Debug ("-With_Attributes");
      end With_Attributes;

      --  19. <attribute-list> --> <attribute-pair> {, <attribute-pair> }
      procedure Attribute_List is
      begin
         Debug ("+Attribute_List");
         -- to be completed!
         Debug ("-Attribute_List");
      end Attribute_List;

      --  20. <attribute-pair> --> <expr-attribute-name> = <expr>
      procedure Attribute_Pair is
      begin
         Debug ("+Attribute_Pair");
         -- to be completed!
         Debug ("-Attribute_Pair");
      end Attribute_Pair;
      
      procedure ID_List is
      begin
         Debug ("+ID_List");
         -- to be completed
         Debug ("-ID_List");
      end ID_List;
      
      --  22. <instance-section> --> instance <instance> { <instance> }
      procedure Instance_Section is
      begin
         Debug ("+Instance_Section");
         -- to be completed!
         Debug ("-Instance_Section");
      end Instance_Section;

      --  23. <instance> --> ID : ID  [ ( <expr-list>  ) ] ;
      procedure Instance is
      begin
         Debug ("+Instance");
         -- to be completed!
         Debug ("-Instance");
      end Instance;
      
      procedure Expr_List is
      begin
         Debug ("+Expr_List");
         -- to be completed
         Debug ("-Expr_List");
      end Expr_List;
        
      --  25. <expr-attribute-name> --> trafficability | vulnerability | range
      --                         | effectiveness | schedule | start | interval
      procedure Expr_Attribute_Name is
      begin
         Debug ("+Expr_Attribute_Name");
         -- to be completed!
         Debug ("-Expr_Attribute_Name");
      end Expr_Attribute_Name;

      --  26. <expr> --> <term> { <addop> <term> }
      procedure Expr is
      begin
         Debug ("+Expr");
         -- to be completed!
         Debug ("-Expr");
      end Expr;

      --  27. <term> --> <signed-factor> { <mulop> <signed-factor> }
      procedure Term is
      begin
         Debug ("+Term");
         -- to be completed!
         Debug ("-Term");
      end Term;

      --  28. <signed-factor> --> - <factor> | <factor>
      procedure Signed_Factor is
      begin
         Debug ("+Signed_Factor");
         -- to be completed!
         Debug ("-Signed_Factor");
      end Signed_Factor;

      --  29. <factor> --> ( <expr> )
      --                 | <random-var>
      --                 | ID [ ( <expr-list>  ) ]
      --                 | NUMBER
      procedure Factor is
      begin
         Debug ("+Factor");
         -- to be completed!
         Debug ("-Factor");
      end Factor;
      
      procedure Random_Var is
      begin
         Debug ("+Random_Var");
         -- to be completed
         Debug ("-Random_Var");
      end Random_Var;
      
      procedure Add_Op is
      begin
         Debug ("+Add_Op");
         -- to be completed
         Debug ("-Add_Op");
      end Add_Op;
      
      procedure Mul_Op is
      begin
         Debug ("+Mul_Op");
         -- to be completed
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
