-- Simple global string table.
-- COL Gene Ressler, 1 December 2007

with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors, Ada.Containers.Hashed_Maps;

package body String_Table is

   -- String pointer type and memory de-allocator.
   type String_Ptr_Type is access all String;
   procedure Free is
     new Ada.Unchecked_Deallocation (String, String_Ptr_Type);

   -- Return true if A and B point to equal string values.
   function Keys_Equal(A, B : in String_Ptr_Type) return Boolean is
   begin
      return A.all = B.all;
   end Keys_Equal;

   -- Set up a vector of string pointers indexed by string handles.
   subtype Non_Null_String_Handle_Type is
     String_Handle_Type range 1 .. String_Handle_Type'Last;

   package String_Vectors is
     new Ada.Containers.Vectors(Index_Type => Non_Null_String_Handle_Type,
                                Element_Type => String_Ptr_Type,
                                "=" => Keys_Equal);
   Strings : String_Vectors.Vector;

   -- Set up a map from strings given by pointers to string handles.

   -- Declare package for maps and then the map.
   function Hash (S : in String_Ptr_Type) return Hash_Type;
   package String_To_Handle_Maps is
     new Ada.Containers.Hashed_Maps(Key_Type => String_Ptr_Type,
                                    Element_Type => String_Handle_Type,
                                    Hash => Hash,
                                    Equivalent_Keys => Keys_Equal,
                                    "=" => "=");
   Map : String_To_Handle_Maps.Map;

   -- Convert a string to corresponding handle.
   function To_Handle (S : in String) return String_Handle_Type is
      S_New        : String_Ptr_Type := new String'(S);
      Position     : String_To_Handle_Maps.Cursor;
      Was_Inserted : Boolean;
   begin
      -- Insert in the global map with next available handle.
      String_To_Handle_Maps.Insert(Map, S_New,
                                   String_Vectors.Last_Index(Strings) + 1,
                                   Position, Was_Inserted);
      -- If insert succeeded, then add the string to the vector,
      -- otherwise, free because it's already there.
      if Was_Inserted then
         String_Vectors.Append(Strings, S_New);
      else
         Free(S_New);
      end if;
      return String_To_Handle_Maps.Element(Position);
   end To_Handle;

   -- Convert handle to corresponding string.
   function To_String(Handle : in String_Handle_Type) return String is
   begin
      if Handle = Null_String_Handle then
         return "[null]";
      else
         return String_Vectors.Element (Strings, Handle).all;
      end if;
   end To_String;

   -- Return a hash value for the handle, just the handle itself!
   function Hash(Handle : in String_Handle_Type) return Hash_Type is
   begin
      return Hash_Type(Handle);
   end Hash;

   -- Famous "hashpjw" function from Aho, Sethi, and Ullman.  It just
   -- adds successive character ASCI codes into the hash value after shifting
   -- the previous hash value left by 4 bits.  Then the top 4 bits back are fed
   -- to the high 4 bits of the lowest byte with an xor operation.  There are
   -- simple reasons this ought to work, and it does well in practice.  It's
   -- also fast.  For very very long strings, you'd want to sample characters
   -- instead of touching every one.
   function Hash(S : in String) return Hash_Type is
      Val : Hash_Type := 0;
   begin
      for I in S'Range loop
         Val := Val * 2 ** 4 + Character'Pos(S(I));
         Val := Val xor ((Val / (2 ** 24)) and 16#F0#);
      end loop;
      return Val;
   end Hash;

   -- Hash function for string pointer keys is easy now.
   function Hash (S : in String_Ptr_Type) return Hash_Type is
   begin
      return Hash (S.all);
   end Hash;

end String_Table;
