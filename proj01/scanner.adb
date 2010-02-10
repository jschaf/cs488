-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Strings;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Text_IO;              use Ada.Text_IO;

with Binary_Search;

package body Scanner is

   package T_IO renames Ada.Text_IO;

   MAX_KEYWORD_LENGTH_C : constant Natural := 24;

   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => MAX_KEYWORD_LENGTH_C);

   -- To_Bounded_String is annoyingly long and To_BS is much more
   -- humorous.
   function To_BS (Source : in String) return SB.Bounded_String is
   begin
      return SB.To_Bounded_String(Source => Source);
   end To_BS;

   type Keyword_Array_T is array (Positive range <>) of SB.Bounded_String;

   KEYWORDS_C : constant Keyword_Array_T :=
     (To_BS("description"),
      To_BS("effectiveness"),
      To_BS("exponential"),
      To_BS("friend"),
      To_BS("instance"),
      To_BS("interval"),
      To_BS("normal"),
      To_BS("number"),
      To_BS("point"),
      To_BS("range"),
      To_BS("responders"),
      To_BS("route"),
      To_BS("schedule"),
      To_BS("segment"),
      To_BS("sensor"),
      To_BS("start"),
      To_BS("threat"),
      To_BS("trafficability"),
      To_BS("trip"),
      To_BS("uniform"),
      To_BS("vulnerability"),
      To_BS("with"));

   -- BLUF: Using separate arrays is a quick, easy, but dirty hack.

   -- If this does not match with KEYWORDS_C we're out of luck.
   -- TOKENS_C and KEYWORDS_C are decoupled because the Binary_Search
   -- package doesn't take an equal function.  Thus, we can't provide
   -- a custom equality test to check specific fields.
   type Token_Array_T is array (Positive range <>) of Token_T;
   TOKENS_C : constant Token_Array_T :=
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
      Keyword_With);

   procedure Keyword_Search is new Binary_Search
     (Element_Type => SB.Bounded_String,
      Index_Type   => Positive,
      Array_Type   => Keyword_Array_T,
      "<"          => SB."<");


   type State_T is
     (Start,
      Saw_Alpha,
      Saw_Underscore,
      Saw_ID,

      Saw_Number,
      Saw_Dot,
      Saw_Digit_Before_Dot,
      Saw_Digit_After_Dot,

      Saw_Dash,
      Saw_Comment,
      Saw_Arrow,
      Saw_Minus,

      Saw_Left_Paren,
      Saw_Right_Paren,
      Saw_Plus,
      Saw_Star,
      Saw_Slash,
      Saw_Tilde,
      Saw_Comma,
      Saw_Equals,
      Saw_Colon,
      Saw_Semi,
      Saw_End_Input,

      Begin_Error,
      Middle_Error,
      End_Error);

   subtype Digit is Character range '0' .. '9';
   subtype Letter_Upper is Character range 'a' .. 'z';
   subtype Letter_Lower is Character range 'A' .. 'Z';

   subtype Table_State_T is State_T range State_T'Range;

   type Transistion_Array_T is array (Table_State_T, Character) of State_T;

   TRANSISTION_TABLE_C : constant Transistion_Array_T :=
     (Start =>
        (Letter_Lower | Letter_Upper => Saw_Alpha,
         ' ' | HT | CR | LF          => Start,

         Digit => Saw_Digit_Before_Dot,
         '.' => Saw_Dot,
         '(' => Saw_Left_Paren,
         ')' => Saw_Right_Paren,
         '+' => Saw_Plus,
         '-' => Saw_Dash,
         '*' => Saw_Star,
         '/' => Saw_Slash,
         '~' => Saw_Tilde,
         ',' => Saw_Comma,
         '=' => Saw_Equals,
         ':' => Saw_Colon,
         ';' => Saw_Semi,
         '$' => Saw_End_Input,
         others => Begin_Error),

      Saw_Alpha =>
        (Letter_Lower | Letter_Upper | Digit => Saw_Alpha,
         '_'                                 => Saw_Underscore,
         others                              => Saw_ID),

      Saw_Underscore =>
        (Letter_Lower | Letter_Upper | Digit  => Saw_Alpha,
         others                               => Begin_Error),

      Saw_ID     => (others => Start),
      Saw_Number => (others => Start),

      Saw_Dot =>
        (Digit  => Saw_Digit_After_Dot,
         others => Begin_Error),

      Saw_Digit_Before_Dot =>
        (Digit => Saw_Digit_Before_Dot,
         '.'        => Saw_Dot,
         others     => Saw_Number),

      Saw_Digit_After_Dot =>
        (Digit  => Saw_Digit_After_Dot,
         '.'    => Begin_Error,
         others => Saw_Number),


      -- Special case because a minus might be the start of an
      -- arrow or comment
      Saw_Dash =>
        ('>'    => Saw_Arrow,
         '-'    => Saw_Comment,
         others => Saw_Minus),

      Saw_Arrow => (others => Start),

      Saw_Comment =>
        (LF     => Start,
         others => Saw_Comment),

      Saw_Minus => (others => Start),

      Saw_Left_Paren  => (others => Start),
      Saw_Right_Paren => (others => Start),
      Saw_Plus        => (others => Start),
      Saw_Star        => (others => Start),
      Saw_Slash       => (others => Start),
      Saw_Tilde       => (others => Start),
      Saw_Comma       => (others => Start),
      Saw_Equals      => (others => Start),
      Saw_Colon       => (others => Start),
      Saw_Semi        => (others => Start),
      Saw_End_Input   => (others => Start),

      Begin_Error =>
        (' ' | '$' | HT | CR | LF => End_Error,
         others                   => Middle_Error),

      -- The only the difference between this and Begin_Error is that
      -- Middle_Error consumes input (i.e. advance).  We need two
      -- states so we don't consume whitespace when the illegal token
      -- is only a single character.
      Middle_Error =>
        (' ' | '$' | HT | CR | LF => End_Error,
         others                   => Middle_Error),

      End_Error => (others => Start)

     );

   type Action_T is record
      Advance      : Boolean;
      Return_Token : Token_T;
   end record;

   type Action_Array_T is array (State_T) of Action_T;

   ACTION_TABLE_C : constant Action_Array_T :=
     (Start => (Advance => True,  Return_Token => Incomplete_Token),

      Saw_Alpha      => (Advance => True,  Return_Token => Incomplete_Token),
      Saw_Underscore => (Advance => True,  Return_Token => Incomplete_Token),
      Saw_ID         => (Advance => False, Return_Token => ID),

      -- Numbers
      Saw_Number           => (Advance => False, Return_Token => Number),
      Saw_Dot              => (Advance => True,
                               Return_Token => Incomplete_Token),
      Saw_Digit_Before_Dot => (Advance => True,
                               Return_Token => Incomplete_Token),
      Saw_Digit_After_Dot  => (Advance => True,
                               Return_Token => Incomplete_Token),

      -- Beginning with a dash
      Saw_Dash    => (Advance => True,  Return_Token => Incomplete_Token),
      Saw_Comment => (Advance => True,  Return_Token => Incomplete_Token),
      Saw_Arrow   => (Advance => True,  Return_Token => Arrow),
      Saw_Minus   => (Advance => False, Return_Token => Minus),

      -- Single characters
      Saw_Left_Paren  => (Advance => True,  Return_Token => Left_Paren),
      Saw_Right_Paren => (Advance => True,  Return_Token => Right_Paren),
      Saw_Plus        => (Advance => True,  Return_Token => Plus),
      Saw_Star        => (Advance => True,  Return_Token => Star),
      Saw_Slash       => (Advance => True,  Return_Token => Slash),
      Saw_Tilde       => (Advance => True,  Return_Token => Tilde),
      Saw_Comma       => (Advance => True,  Return_Token => Comma),
      Saw_Equals      => (Advance => True,  Return_Token => Equals),
      Saw_Colon       => (Advance => True,  Return_Token => Colon),
      Saw_Semi        => (Advance => True,  Return_Token => Semi),
      Saw_End_Input   => (Advance => False, Return_Token => End_Input),

      Begin_Error  => (Advance => False, Return_Token => Incomplete_Token),
      Middle_Error => (Advance => True,  Return_Token => Incomplete_Token),
      End_Error    => (Advance => False, Return_Token => Illegal_Token)
     );

   ---------------------
   -- Scan_Next_Token --
   ---------------------

   procedure Scan_Next_Token
     (S           : in     String;
      Start_Index :    out Positive;
      End_Index   : in out Natural;
      Line_Number : in out Positive;
      Token       :    out Token_T)
   is
      Peek_Index : Natural := End_Index + 1;

      function Peek return Character is
      begin
         if Peek_Index > S'Last then
            return '$';
         else
            return S(Peek_Index);
         end if;
      end Peek;

      procedure Advance is
      begin
         Peek_Index := Peek_Index + 1;
      end Advance;


      State, New_State : State_T := Start;

      Return_Token : Token_T;

      -- For differentiating IDs and keywords
      Found_Keyword    : Boolean    := False;
      Keyword_Index    : Integer;
      Keyword          : SB.Bounded_String;
      Search_Keyword   : SB.Bounded_String;

   begin

      Scanner_Loop:
      loop
         New_State := TRANSISTION_TABLE_C(State, Peek);

         if New_State = End_Error then
            Token := Illegal_Token;
            exit Scanner_Loop;
         end if;

         if State = Start and then New_State /= Start then
            Start_Index := Peek_Index;
         end if;

         if ACTION_TABLE_C(New_State).Advance then
            if Peek = LF then
               Line_Number := Line_Number + 1;
            end if;
            Advance;
            End_Index := End_Index + 1;
         end if;

         Return_Token := ACTION_TABLE_C(New_State).Return_Token;

         -- Beware of DeMorgan
         if (Return_Token /= Incomplete_Token
               and then Return_Token /= Illegal_Token) then
            Token := Return_Token;

            -- Don't check strings that are longer than the max
            -- keyword length because they obviously cannot be
            -- keywords.
            if (End_Index - Start_Index) > MAX_KEYWORD_LENGTH_C then
               exit Scanner_Loop;
            end if;

            Search_Keyword := To_BS(S(Start_Index .. End_Index));

            Keyword_Search(Elements => KEYWORDS_C,
                           Search   => Search_Keyword,
                           Found    => Found_Keyword,
                           Index    => Keyword_Index);

            if Found_Keyword then
               Keyword := KEYWORDS_C(Keyword_Index);
               Token   := TOKENS_C(Keyword_Index);
            end if;

            exit Scanner_Loop;
         end if;

         State := New_State;

      end loop Scanner_Loop;
   end Scan_Next_Token;

   -- Eliminate the leading space that Ada puts in front of positive
   -- integer images.
   function Image(N : in Integer) return String is
      S : String := Integer'Image(N);
   begin
      if S(1) = ' ' then
         return S(2 .. S'Last);
      end if;
      return S;
   end Image;

   -- Print scanner return as a human-readable string.
   procedure Put_Scanned_Token
     (S           : in String;
      Start_Index : in Positive;
      End_Index   : in Natural;
      Line_Number : in Positive;
      Token       : in Token_T;
      Indent      : in Natural := 0)
   is
   begin
      T_IO.Put(Indent * ' ' & "[" & Image(Line_Number) & "|" &
                 S(Start_Index .. End_Index) & "|"
                   & Token_T'Image(Token) & "]");
   end Put_Scanned_Token;

   package String_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Character);

   -- Rewritten to take advantage of Vectors.
   function Read_To_String (File_Name : in Buffer_T) return Buffer_A is

      function Vec_To_Buffer_A 
        (Vec : in String_Vector.Vector) 
        return Buffer_A 
      is
         Full_String : String(1 .. Positive(Vec.Length));
      begin
         for I in Full_String'Range loop
            Full_String(I) := Vec.Element(I);
         end loop;
         return new Buffer_T'(Full_String);
      end Vec_To_Buffer_A;

      Infile : File_Type;
      -- To avoid many vector resizes
      VECTOR_INITIAL_SIZE_C : constant := 1000;
      Char_Vector : String_Vector.Vector
        := String_Vector.To_Vector(VECTOR_INITIAL_SIZE_C);
   begin
      Open(File => Infile, Mode => In_File, Name => File_Name);
      loop
         Char_Vector.Append(Character'Input(Stream(Infile)));
      end loop;
   exception
   when T_IO.End_Error =>
      Close(Infile);
      return Vec_To_Buffer_A(Char_Vector);
   end Read_To_String;

end Scanner;
