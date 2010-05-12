-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Command_Line, Ada.Strings.Unbounded, Ada.Text_IO, Ada.Long_Float_Text_IO;
use  Ada.Command_Line, Ada.Strings.Unbounded, Ada.Text_IO, Ada.Long_Float_Text_IO;

with Common, Scanner, Parser, AST, AST.Tables, AST.Trees, Simulator, Animation;
use  Common, Scanner, Parser, AST, AST.Tables, AST.Trees, Simulator, Animation;

procedure MIEDS is

   MIEDS_Code          : Buffer_Ptr_Type;
   Tree, Expanded_Tree : Node_Ptr_Type;
   Symbol_Table        : Symbol_Table_Ptr_Type := new Symbol_Table_Type;
   State               : Simulation_State_Type;
   Input_File_Name     : Unbounded_String;
   Debug_Mode          : Boolean;
   Animation_Enabled   : Boolean;
   End_Time            : Real;
   Last                : Positive;
   
   -- Here is a case of using a DFA to simplify a mundane task!
   type Arg_Processing_State_Type is (Waiting_Option, Waiting_End_Time);
   Arg_Processing_State : Arg_Processing_State_Type := Waiting_Option;

   Error_Exit, Argument_Error : exception;
begin
   if Argument_Count = 0 then
      Put_Line("Usage: " & Command_Name & " [-d] [-a] [-e Time] FileName");
      Put_Line("  -d       Produce debugging output from scanner, parser, and expander.");
      Put_Line("  -a       Enable animation of the simulation as the simulation runs.");
      Put_Line("  -e Time  Set end time of simulation to Time (default is never).");
      Put_Line("  FileName MIEDS source program.");
      raise Error_Exit;
   end if;

   -- Set default values for run parameters.
   Input_File_Name   := Null_Unbounded_String;
   Debug_Mode        := False;
   Animation_Enabled := False;
   End_Time          := Real'Last;

   -- For loop processes arguments in sequence.
   for I in 1 .. Argument_Count loop
      -- Branch on state.
      case Arg_Processing_State is
         when Waiting_Option =>
            if Argument(I) = "-d" then
               Debug_Mode := True;
            elsif Argument(I) = "-a" then
               Animation_Enabled := True;
            elsif Argument(I) = "-e" then
               Arg_Processing_State := Waiting_End_Time;
            else
               Input_File_Name := To_Unbounded_String(Argument(I));
            end if;
         when Waiting_End_Time =>
            Get(Argument(I), End_Time, Last);
            Arg_Processing_State := Waiting_Option;
      end case;
   end loop;
   -- Note missing file name error.
   if Input_File_Name = Null_Unbounded_String then
      raise Argument_Error with "missing input file name";
   end if;
   -- Note if in a state where an arg value is still expected.
   if Arg_Processing_State /= Waiting_Option then
      raise Argument_Error with "missing argument value";
   end if;

   -- Enable or disable animation.
   Animation.Enable(Animation_Enabled);

   -- Read the example file to a string.
   Read_To_String(To_String(Input_File_Name), MIEDS_Code);

   -- Run the parser on it.
   Parse(S          => MIEDS_Code.all,
         Rtn_Tree   => Tree,
         Debug_Mode => Debug_Mode);

   -- Do macro expansion.
   Expand(Tree, Symbol_Table, Expanded_Tree);

   -- Print macro expanded tree if in debug mode.
   if Debug_Mode then
      Put_As_XML(Expanded_Tree);
   end if;

   -- Finish the tree: Collapse lambdas and compute cached information.
   Finish(Expanded_Tree);

   -- Initialize and run the simulator.
   Initialize(State, Expanded_Tree);
   Run(State, End_Time);
exception
   when Error_Exit =>
      Set_Exit_Status(Failure);
end MIEDS;
