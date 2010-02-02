-- Not my code!
-- 
-- Copied verbatim from:
-- http://en.wikibooks.org/wiki/Algorithm_Implementation/Search/Binary_search

procedure Binary_Search
  (Elements : in Array_Type;
   Search   : in Element_Type;
   Found    : out Boolean;
   Index    : out Index_Type'Base)
is
   Left  : Index_Type'Base := Elements'First;
   Right : Index_Type'Base := Elements'Last + 1;
begin
   if Search < Elements (Left) then
      Index := Left - 1;
      Found := False;
   else
      loop
         declare
            Center    : constant Index_Type   :=
              Left + (Right - Left) / 2;
            Candidate : constant Element_Type :=
              Elements (Center);
         begin
            if Search = Candidate then
               Index := Center;
               Found := True;
               exit;
            end if;

            if Right - Left <= 1 then
               Index := Center;
               Found := False;
               exit;
            elsif Search < Candidate then
               Right := Center;
            else
               Left := Center;
            end if;
         end;
      end loop;
   end if;
   return;
end Binary_Search;
