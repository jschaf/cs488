-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Unchecked_Deallocation;

package Scanner is

   Unexepected_Character : exception;

   -- Tokens recognized by the scanner.
   type Token_T is
     (Keyword_Description,
      Keyword_Effectiveness,
      Keyword_Exponential,
      Keyword_Friend,
      Keyword_Instance,
      Keyword_Interval,
      Keyword_Normal,
      Keyword_Number,
      Keyword_Point,
      Keyword_Range,
      Keyword_Responders,
      Keyword_Route,
      Keyword_Schedule,
      Keyword_Segment,
      Keyword_Sensor,
      Keyword_Start,
      Keyword_Threat,
      Keyword_Trafficability,
      Keyword_Trip,
      Keyword_Uniform,
      Keyword_Vulnerability,
      Keyword_With,

      Arrow,
      ID,
      Number,
      Left_Paren,
      Right_Paren,
      Plus,
      Minus,
      Underscore,
      Star,
      Slash,
      Tilde,
      Comma,
      Equals,
      Colon,
      Semi,
      End_Input,
      Incomplete_Token,
      Illegal_Token);

   -- Scan one token from the given string starting at End_Index+1
   -- and setting both Start_Index and End_Index to denote the next token.
   -- Line number is incremented each time a linefeed is seen in the input.
   -- With this convention, the scanner can be called repeatedly to
   -- scan all the tokens in the string. For example:
   --    ... yada yada
   --    Token : Token_Type;
   --    Line_Number : Positive := 1;
   --    Start_Index : Positive := 1;
   --    End_Index : Natural := 0;
   -- begin
   --    loop
   --       Scan_Next_Token(S, Start_Index, End_Index, Token);
   --       exit when Token = End_Input_Token;
   --    end loop;
   -- end ...;

   procedure Scan_Next_Token
     (S           : in     String;
      Start_Index :    out Positive;
      End_Index   : in out Natural;
      Line_Number : in out Positive;
      Token       :    out Token_T);

   -- For debugging purposes, this prints scanned token information to
   -- the screen.
   procedure Put_Scanned_Token 
     (S           : in String;
      Start_Index : in Positive;
      End_Index   : in Natural;
      Line_Number : in Positive;
      Token       : in Token_T;
      Indent      : in Natural  := 0);

   subtype Buffer_T is String;
   type Buffer_A is access Buffer_T;

   -- Read an entire file into a single string.  Lines are separated by
   -- the "line feed character".  If you say
   --
   -- with Ada.Characters.Latin_1;
   -- use  Ada.Characters.Latin_1;
   --
   -- then the identifier LF is the name of this character.  For example,
   -- you can say:
   --
   -- case Peek is
   -- when LF =>  ... do what you want when peek is a line feed ...
   --
   function Read_To_String (File_Name : in Buffer_T) return Buffer_A;

end Scanner;
