with Ada.Text_IO;
use  Ada.Text_IO;

with Scanner;
use  Scanner;

procedure Scanner_Develop is
   S : Buffer_A := Read_To_String("example.sim");
   Start_Index, Line_Number, Last_Line_Number : Positive := 1;
   End_Index : Natural := 0;
   Token : Token_T;
begin
   
   Put_Line(S.all);
   
   loop
      Scan_Next_Token(S.all, Start_Index, End_Index, Line_Number, Token);
      if Line_Number > Last_Line_Number then
         New_Line(2);
      end if;
      Put_Scanned_Token(S.all, Start_Index, End_Index, Line_Number, Token);
      exit when Token = End_Input;
      Last_Line_Number := Line_Number;
   end loop;
end Scanner_Develop;
