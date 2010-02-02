-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Characters.Latin_1, Ada.Strings.Fixed;
use  Ada.Text_IO, Ada.Characters.Latin_1, Ada.Strings.Fixed;

with Binary_Search;

with Ada.Containers.Generic_Array_Sort;

package body Scanner is
   
   -- Index subtype ranges over possible bounded string lengths.
   subtype Keyword_Index_Type is Natural range 0 .. 24;

   -- Keyword table entry takes string length as a discriminant, which
   --  is a parameter for a _type_ rather than a procedure.
   type Keyword_Tuple(Length : Keyword_Index_Type := 0) is
      record
         Value : String(1 .. Length);
         Token : Token_Type;
      end record;
   
   function Keyword_Less_Than 
     (Left, Right : in Keyword_Tuple)
     return Boolean 
   is
   begin
      return Left.Value < Right.Value;
   end Keyword_Less_Than;
   
   --  Function accepts string and token and returns a keyword table
   --  entry all filled in with a string of correct length.
   function Keyword_Entry 
     (S     : in String; 
      Token : in Token_Type)
     return Keyword_Tuple 
   is
   begin
      return Keyword_Tuple'(Length => S'Length, 
                            Value  => S, 
                            Token  => Token);
   end Keyword_Entry;

   type Keyword_Table_Type is
     array(Positive range <>) of Keyword_Tuple;
   
   procedure Keyword_Sort is new Ada.Containers.Generic_Array_Sort
     (Element_Type => Keyword_Tuple,
      Index_Type   => Positive,
      Array_Type   => Keyword_Table_Type,
      "<"          => Keyword_Less_Than);
   
   function Key_Sort 
     (Keys : in Keyword_Table_Type) 
     return Keyword_Table_Type 
   is
      K : Keyword_Table_Type := Keys;
   begin
      Keyword_Sort(K);
      return K;
   end Key_Sort;
   
   -- Sort this just to play it safe.
   Keyword_Table : constant Keyword_Table_Type :=
     Key_Sort(  
       (Keyword_Entry("description",     Keyword_Description),
        Keyword_Entry("effectiveness",   Keyword_Effectiveness),
        Keyword_Entry("exponential",     Keyword_Exponential),
        Keyword_Entry("friend",          Keyword_Friend),
        Keyword_Entry("instance",        Keyword_Instance),
        Keyword_Entry("interval",        Keyword_Interval),
        Keyword_Entry("normal",          Keyword_Normal),
        Keyword_Entry("number",          Keyword_Number),
        Keyword_Entry("point",           Keyword_Point),
        Keyword_Entry("range",           Keyword_Range),
        Keyword_Entry("responders",      Keyword_Responders),
        Keyword_Entry("route",           Keyword_Route),
        Keyword_Entry("schedule",        Keyword_Schedule),
        Keyword_Entry("segment",         Keyword_Segment),
        Keyword_Entry("sensor",          Keyword_Sensor),
        Keyword_Entry("start",           Keyword_Start),
        Keyword_Entry("threat",          Keyword_Threat),
        Keyword_Entry("trafficability",  Keyword_Trafficability),
        Keyword_Entry("trip",            Keyword_Trip),
        Keyword_Entry("uniform",         Keyword_Uniform),
        Keyword_Entry("vulnerability",   Keyword_Vulnerability),
        Keyword_Entry("with",            Keyword_With)));
                      
   procedure Keyword_Search is new Binary_Search
     (Element_Type => Keyword_Tuple,
      Index_Type   => Positive,
      Array_Type   => Keyword_Table_Type,
      "<"          => Keyword_Less_Than);
   
   ---------------------
   -- Scan_Next_Token --
   ---------------------

   procedure Scan_Next_Token
     (S           : in     String;
      Start_Index :    out Positive;
      End_Index   : in out Natural;
      Line_Number : in out Positive;
      Token       :    out Token_Type)
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
      
      type State_Type is 
        (Start, 
         Saw_Alpha, 
         Saw_Underscore, 
         Saw_ID,
         Saw_Number,
         Saw_Left_Paren,
         Saw_Right_Paren,
         Saw_Plus,
         Saw_Minus,
         Saw_Arrow,
         Saw_Star,
         Saw_Slash,
         Saw_Tilde,
         Saw_Comma,
         Saw_Equals,
         Saw_Colon,
         Saw_Semi,
         Saw_End_Input,
         Error);
      
      -- Exclude Error state.
      subtype Table_State_Type is State_Type range Start .. Saw_End_Input;
      
      type Transistion_Array is array (Table_State_Type, Character) of State_Type;
      
      Transistion_Table : Transistion_Array :=
        (Start => 
           ('a' .. 'z' | 'A' .. 'Z' => Saw_Alpha,
            ' ' | HT                => Start,
            
            '(' => Saw_Left_Paren,
            ')' => Saw_Right_Paren,
            '+' => Saw_Plus,
            '-' => Saw_Minus,
            '*' => Saw_Star,
            '/' => Saw_Slash,
            '~' => Saw_Tilde,
            ',' => Saw_Comma,
            '=' => Saw_Equals,
            ':' => Saw_Colon,
            ';' => Saw_Semi,
            '$' => Saw_End_Input,
            
            others                  => Error),
         
         Saw_Alpha => 
           ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => Saw_Alpha,
            '_'                                  => Saw_Underscore,
            ' ' | HT                             => Saw_ID,
            others                               => Error),
         
         Saw_Underscore => 
           ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => Saw_Alpha,
            others                               => Error),
         
         Saw_ID     => (others => Start),
         Saw_Number => (others => Start), -- XXX: Implement me
         
         -- Special case because a minus might be the start of an
         -- arrow.
         Saw_Minus => 
           ('>'    => Saw_Arrow,
            others => Start),
         
         Saw_Arrow       => (others => Start),
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
         Saw_End_Input   => (others => Start)
        );
      
      type Action_Type is record
         Advance : Boolean;
         Return_Token : Token_Type;
      end record;
      
      type Action_Array is array (State_Type) of Action_Type;
      
      Action_Table : Action_Array :=
        (Saw_ID => (Advance => False, Return_Token => ID),
         
         Saw_Arrow       => (Advance => True, Return_Token => Arrow),
         Saw_Left_Paren  => (Advance => True, Return_Token => Left_Paren),
         Saw_Right_Paren => (Advance => True, Return_Token => Right_Paren),
         Saw_Plus        => (Advance => True, Return_Token => Plus),
         Saw_Star        => (Advance => True, Return_Token => Star),
         Saw_Slash       => (Advance => True, Return_Token => Slash),
         Saw_Tilde       => (Advance => True, Return_Token => Tilde),
         Saw_Comma       => (Advance => True, Return_Token => Comma),
         Saw_Equals      => (Advance => True, Return_Token => Equals),
         Saw_Colon       => (Advance => True, Return_Token => Colon),
         Saw_Semi        => (Advance => True, Return_Token => Semi),
         Saw_End_Input   => (Advance => True, Return_Token => End_Input),

         others => (Advance => True,  Return_Token => Illegal_Token));
      
      State, New_State : State_Type := Start;
      
      function Keyword_Or_ID (Token : in Token_Type; 
                              Start, Stop : in Natural) return Token_Type is
         Str : String := S(Start .. Stop);
      begin
         if Token /= ID then
            return Token;
         end if;
         
         if Str = "description" then
            return Keyword_Description;
         elsif Str = "effectiveness" then
            return Keyword_Effectiveness;
         else
            return ID;
         end if;
      end Keyword_Or_ID;
   
   begin
      loop
         New_State := Transistion_Table(State, Peek);
         
         if New_State = Error then
            raise Unexepected_Character;
         end if;
         
         if State = Start and New_State /= Start then
            Start_Index := Peek_Index;
         end if;
         
         if Action_Table(New_State).Advance then
            Advance;
         end if;
         
         if Action_Table(New_State).Return_Token /= Illegal_Token then
            Token := Action_Table(New_State).Return_Token;
            -- XXX: Use Keyword Search
            return;
         end if;
         
         State := New_State;
         
      end loop;
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
      Token       : in Token_Type;
      Indent      : in Natural := 0)
   is
   begin
      Put(Indent * ' ' & "[" & Image(Line_Number) & "|" &
            S(Start_Index .. End_Index) & "|" & Token_Type'Image(Token) & "]");
   end Put_Scanned_Token;

   --  Slurp an entire file into a buffer.  Return a pointer to the
   --  buffer string or null of the file could not be opened.  Lines
   --  are separated with Ada.Characters.Latin_1.LF characters.
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

end Scanner;
