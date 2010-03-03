-- Simple global string table.
-- COL Gene Ressler, 1 December 2007

with Ada.Containers; use Ada.Containers;

package String_Table is

   -- A string handle is just a non-negative integer.
   type String_Handle_Type is new Natural;

   -- Sometimes we want to set a string handle variable to something that
   -- isn't a valid handle.  This is the value to use.
   Null_String_Handle : constant String_Handle_Type := 0;

   -- Convert a string to its handle.  The string is added to the
   -- table if it isn't already there.  Otherwise the existing handle
   -- is looked up and returned.
   function To_Handle(S : in String) return String_Handle_Type;

   -- Convert a handle to a string.
   function To_String(Handle : in String_Handle_Type) return String;

   -- Hash a string handle.  For use in Ada.Containers.
   function Hash(Handle : in String_Handle_Type) return Hash_Type;

end String_Table;
