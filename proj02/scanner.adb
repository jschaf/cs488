-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Characters.Latin_1, Ada.Strings.Fixed;
use  Ada.Text_IO, Ada.Characters.Latin_1, Ada.Strings.Fixed;

package body Scanner is

   -- Look up a string in the keyword table.  If found, return its
   -- corresponding token, otherwise return Id.  Algorithm is binary search.
   -- Declared here.  Implementation at end of package.
   function Keyword_Token(S : in String) return Token_Type;

   ---------------------
   -- Scan_Next_Token --
   ---------------------

   procedure Scan_Next_Token
     (S           : in String;
      Start_Index : out Positive;
      End_Index   : in out Natural;
      Line_Number : in out Positive;
      Token       : out Token_Type)
   is
      -- Start by peeking at the character after last token scanned.
      Peek_Index : Natural := End_Index + 1;

      -- Peek at next character on the input.  Return a special character when
      -- we read past the input.
      function Peek return Character is
      begin
         if Peek_Index > S'Last then
            return '$';
         else
            return S(Peek_Index);
         end if;
      end Peek;

      -- Move on to peek at the next character.
      procedure Advance is
      begin
         Peek_Index := Peek_Index + 1;
      end Advance;

      -- Advance until we're peeking at whitespace or else the end of input.
      -- Set end-index at the last character read.
      procedure Purge is
      begin
         loop
            case Peek is
               when ' ' | HT | LF | '$' => return;
               when others => Advance;
            end case;
         end loop;
      end Purge;

      -- DFA states.
      type State_Type is
        (Start, Saw_Minus, In_Comment, Saw_Digit, In_Trail_Digits, Saw_Dot,
         Saw_Letter, Saw_Underscore);

      -- Current state of the scanner.
      State : State_Type := Start;
   begin
      loop
         case State is
            when Start =>
               Start_Index := Peek_Index;
               case Peek is
                  when ' ' | HT => -- HT is horizontal tab
                     Advance;
                  when LF => -- LF is linefeed
                     Line_Number := Line_Number + 1;
                     Advance;
                  when '-' =>
                     Advance;
                     State := Saw_Minus;
                  when '0' .. '9' =>
                     Advance;
                     State := Saw_Digit;
                  when '.' =>
                     Advance;
                     State := Saw_Dot;
                  when 'a' .. 'z' | 'A' .. 'Z' =>
                     Advance;
                     State := Saw_Letter;
                  when '$' => -- end of input marker
                     End_Index := Start_Index - 1;
                     Token := End_Input;
                     return;
                  when '(' =>
                     End_Index := Start_Index;
                     Token := Left_Paren;
                     return;
                  when ')' =>
                     End_Index := Start_Index;
                     Token := Right_Paren;
                     return;
                  when '+' =>
                     End_Index := Start_Index;
                     Token := Plus;
                     return;
                  when '_' =>
                     End_Index := Start_Index;
                     Token := Underscore;
                     return;
                  when '*' =>
                     End_Index := Start_Index;
                     Token := Star;
                     return;
                  when '/' =>
                     End_Index := Start_Index;
                     Token := Slash;
                     return;
                  when '~' =>
                     End_Index := Start_Index;
                     Token := Tilde;
                     return;
                  when ',' =>
                     End_Index := Start_Index;
                     Token := Comma;
                     return;
                  when '=' =>
                     End_Index := Start_Index;
                     Token := Equals;
                     return;
                  when ':' =>
                     End_Index := Start_Index;
                     Token := Colon;
                     return;
                  when ';' =>
                     End_Index := Start_Index;
                     Token := Semi;
                     return;
                  when others =>
                     End_Index := Start_Index;
                     Token := Illegal_Token;
                     return;
               end case;
            when Saw_Minus =>
               case Peek is
                  when '-' =>
                     Advance;
                     State := In_Comment;
                  when '>' =>
                     End_Index := Peek_Index;
                     Token := Arrow;
                     return;
                  when others =>
                     End_Index := Start_Index;
                     Token := Minus;
                     return;
               end case;
            when In_Comment =>
               case Peek is
                  when LF | '$' =>
                     State := Start;
                  when others =>
                     Advance;
               end case;
            when Saw_Digit =>
               case Peek is
                  when '.' =>
                     Advance;
                     State := In_Trail_Digits;
                  when '0' .. '9' =>
                     Advance;
                  when others =>
                     End_Index := Peek_Index - 1;
                     Token := Number;
                     return;
               end case;
            when In_Trail_Digits =>
               if Peek in '0' .. '9' then
                  Advance;
               else
                  End_Index := Peek_Index - 1;
                  Token := Number;
                  return;
               end if;
            when Saw_Dot =>
               if Peek in '0' .. '9' then
                  Advance;
                  State := In_Trail_Digits;
               else
                  Purge;
                  End_Index := Peek_Index - 1;
                  Token := Illegal_Token;
                  return;
               end if;
            when Saw_Letter =>
               case Peek is
                  when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' =>
                     Advance;
                  when '_' =>
                     Advance;
                     State := Saw_Underscore;
                  when others =>
                     End_Index := Peek_Index - 1;
                     Token := Keyword_Token( S(Start_Index .. End_Index) );
                     return;
               end case;
            when Saw_Underscore =>
               case Peek is
                  when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' =>
                     Advance;
                     State := Saw_Letter;
                  when others =>
                     Purge;
                     End_Index := Peek_Index - 1;
                     Token := Illegal_Token;
                     return;
               end case;
         end case;
      end loop;
   end Scan_Next_Token;

   -----------------------
   -- Put_Scanned_Token --
   -----------------------

   -- Eliminate the silly leading space that Ada puts in front of positive
   -- integer images.
   function Integer_Image(N : in Integer) return String is
      S : String := Integer'Image(N);
   begin
      if S(1) = ' ' then
         return S(2 .. S'Last);
      end if;
      return S;
   end Integer_Image;

   -- Print scanner return as a human-readable string.
   procedure Put_Scanned_Token(S : in String;
                               Start_Index : in Positive;
                               End_Index : in Positive;
                               Line_Number : in Positive;
                               Token : in Token_Type;
                               Indent : in Natural := 0)
   is
   begin
      Put(Indent * ' ' & "[" & Integer_Image(Line_Number) & "|" &
          S(Start_Index .. End_Index) & "|" & Token_Type'Image(Token) & "]");
   end Put_Scanned_Token;

   --------------------
   -- Read_To_String --
   --------------------

   -- Slurp an entire file into a buffer.  Return a pointer to the buffer
   -- string or null of the file could not be opened.  Lines are
   -- separated with Ada.Characters.Latin_1.LF characters.
   procedure Read_To_String
     (File_Name : in String;
      S : out Buffer_Ptr_Type)
   is
      F : File_Type;
      Buf : Buffer_Ptr_Type;
      Last_Index, Get_Index : Natural := 0;

      procedure Extend_Buffer(New_Min_Size : in Natural) is
         Tmp : Buffer_Ptr_Type;
         New_Last : Natural := Buf'Last;
      begin
         while New_Last < New_Min_Size loop
           New_Last := 2 * New_Last;
         end loop;
         if New_Last > Buf'Last then
           Tmp := new String(1 .. New_Last);
           Tmp(Buf'Range) := Buf.all;
           Free(Buf);
           Buf := Tmp;
         end if;
      end Extend_Buffer;

   begin
      Open(F, In_File, File_Name);
      -- Allocate initial buffer.
      Buf := new String(1 .. 2);
      -- First get_line starts at position 1.
      Get_Index := 1;
      while not End_Of_File(F) loop
         loop
            Get_Line(F, Buf(Get_Index .. Buf'Last), Last_Index);
            -- Exit when end of line or end of file. The end-of-file
            -- should never happen in Unix because Unix files always
            -- have an end-of-line as last character, but Windows does
            -- not require it.
            exit when Last_Index < Buf'Last or End_Of_File(F);
            -- Next get_line starts after the last char of this one.
            Get_Index := Last_Index + 1;
            -- Make the buffer bigger.
            Extend_Buffer(Get_Index);
         end loop;
         -- Make room for line feed and at least one character.
         Extend_Buffer(Last_Index + 2);
         Last_Index := Last_Index + 1;
         Buf(Last_Index) := LF;
         Get_Index := Last_Index + 1;
      end loop;
      S := new String'(Buf(1 .. Last_Index));
      Free(Buf);
      Close(F);
   exception
      when Name_Error =>
         S := null;
   end Read_To_String;

   -- Could also use a Bounded_String of length 24, but I've done it this way to
   -- show you how Bounded_String is implemented inside.

   -- Index subtype ranges over possible bounded string lengths.
   subtype Keyword_Index_Type is Natural range 0 .. 24;

   -- Keyword table entry takes string length as a discriminant, which is
   -- essentially a parameter for a _type_ rather than a procedure.
   type Keyword_Table_Entry_Type(Length : Keyword_Index_Type := 0) is
      record
         Value : String(1 .. Length);
         Token : Token_Type;
      end record;

   -- Function accepts string and token and returns a keyword table entry all
   -- filled in with a string of correct length.
   function Keyword_Entry(S : String; Token : Token_Type)
                          return Keyword_Table_Entry_Type is
   begin
      return (Length => S'Length, Value => S, Token => Token);
   end Keyword_Entry;

   type Keyword_Table_Type is
     array(Positive range <>) of Keyword_Table_Entry_Type;

   -- Keyword table must be in alphabetical order for binary search.
   Keyword_Table : constant Keyword_Table_Type :=
     (Keyword_Entry("description", 	Keyword_Description),
      Keyword_Entry("duration",         Keyword_Duration),
      Keyword_Entry("effectiveness", 	Keyword_Effectiveness),
      Keyword_Entry("exponential", 	Keyword_Exponential),
      Keyword_Entry("friend",		Keyword_Friend),
      Keyword_Entry("instance", 	Keyword_Instance),
      Keyword_Entry("interval", 	Keyword_Interval),
      Keyword_Entry("normal", 		Keyword_Normal),
      Keyword_Entry("number", 		Keyword_Number),
      Keyword_Entry("point", 		Keyword_Point),
      Keyword_Entry("range",		Keyword_Range),
      Keyword_Entry("route", 		Keyword_Route),
      Keyword_Entry("schedule", 	Keyword_Schedule),
      Keyword_Entry("segment", 		Keyword_Segment),
      Keyword_Entry("sensor", 		Keyword_Sensor),
      Keyword_Entry("start", 		Keyword_Start),
      Keyword_Entry("threat", 		Keyword_Threat),
      Keyword_Entry("trafficability", 	Keyword_Trafficability),
      Keyword_Entry("trip",		Keyword_Trip),
      Keyword_Entry("uniform", 		Keyword_Uniform),
      Keyword_Entry("vulnerability", 	Keyword_Vulnerability),
      Keyword_Entry("with", 		Keyword_With));

   -- Look up a string in the keyword table with binary search. If found, return
   -- its corresponding token, otherwise return Id.
   function Keyword_Token(S : in String) return Token_Type is
      Lo : Natural := Keyword_Table'First;
      Hi : Natural := Keyword_Table'Last;
      Mid : Natural;
   begin
      while Lo <= Hi loop
         Mid := (Lo + Hi) / 2;
         if S < Keyword_Table(Mid).Value then
            Hi := Mid - 1;
         elsif S > Keyword_Table(Mid).Value then
            Lo := Mid + 1;
         else
            return Keyword_Table(Mid).Token;
         end if;
      end loop;
      return Id;
   end Keyword_Token;

end Scanner;
