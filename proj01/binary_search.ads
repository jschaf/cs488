-- Not my code!
-- 
-- Copied verbatim from:
-- http://en.wikibooks.org/wiki/Algorithm_Implementation/Search/Binary_search

generic
   type Element_Type is private;
   type Index_Type is range <>;
   type Array_Type is array (Index_Type range <>) of Element_Type;
   with function "<"
     (Left  : in Element_Type;
      Right : in Element_Type)
     return Boolean is <>;

procedure Binary_Search
  (Elements : in Array_Type;
   Search   : in Element_Type;
   Found    : out Boolean;
   Index    : out Index_Type'Base);
