-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Characters.Latin_1, Ada.Strings.Fixed;
use  Ada.Text_IO, Ada.Characters.Latin_1, Ada.Strings.Fixed;

package body Scanner is

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
      
      type State_Type is (Start, Saw_Alpha, Saw_Underscore, Saw_ID, Error);
      
      subtype Table_State_Type is State_Type range Start .. Saw_Underscore;
      
      type Transistion_Array is array (Table_State_Type, Character) of State_Type;
      
      Transistion_Table : Transistion_Array :=
        (Start => 
           ('a' .. 'z' | 'A' .. 'Z' => Saw_Alpha,
            ' ' | HT                => Start,
            others                  => Error),
         
         Saw_Alpha => 
           ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => Saw_Alpha,
            '_'                                  => Saw_Underscore,
            ' ' | HT                             => Saw_ID,
            others                               => Error),
         
         Saw_Underscore => 
           ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => Saw_Alpha,
            others                               => Error));
      
      type Action_Type is record
         Advance : Boolean;
         Return_Token : Token_Type;
      end record;
      
      type Action_Array is array (State_Type) of Action_Type;
      
      Action_Table : Action_Array :=
        (Saw_ID => (Advance => False, Return_Token => ID),
         others => (Advance => True,  Return_Token => Illegal_Token));
      
      State, New_State : State_Type := Start;
   
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
