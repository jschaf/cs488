-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Command_Line, Ada.Strings.Unbounded, Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Command_Line, Ada.Strings.Unbounded, Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO;
use Ada.Long_Float_Text_IO;

with Common, Scanner, Parser, AST, AST.Tables, AST.Trees, Simulator;
use  Common, Scanner, Parser, AST, AST.Tables, AST.Trees, Simulator;

procedure MIEDS is

   MIEDS_Code          : Buffer_Ptr_Type;
   Tree                : Node_Ptr_Type;
-- Expanded_Tree       : Node_Ptr_Type;
   Symbol_Table        : Symbol_Table_Ptr_Type;
   State               : Simulation_State_Type;
   Input_File_Name     : Unbounded_String;
   Debug_Mode          : Boolean;
   Type_Debug_Mode     : Boolean;
   Animation_Enabled   : Boolean;
   End_Time            : Real;
   N_Runs              : Positive;
   Last                : Positive;

   -- Here is a case of using a DFA to simplify a mundane task!
   type Arg_Processing_State_Type is (Waiting_Option, Waiting_End_Time, Waiting_N_Runs);
   Arg_Processing_State : Arg_Processing_State_Type := Waiting_Option;

   Error_Exit, Argument_Error : exception;
begin
   if Argument_Count = 0 then
      Put_Line("Usage: " & Command_Name & " [-d] [-a] [-e Time] FileName");
      Put_Line("  -d       Produce debugging output from scanner, parser, and expander.");
      Put_Line("  -D       Produce debugging output from type checker.");
      Put_Line("  -a       Enable animation of the simulation as the simulation runs.");
      Put_Line("  -e Time  Set end time of simulation to Time (default is never).");
      Put_Line("  -r Count Run the scenario Count times (default is 1; -e required).");
      Put_Line("  FileName MIEDS source program.");
      raise Error_Exit;
   end if;

   -- Set default values for run parameters.
   Input_File_Name   := Null_Unbounded_String;
   Debug_Mode        := False;
   Type_Debug_Mode   := False;
   Animation_Enabled := False;
   End_Time          := Real'Last;
   N_Runs            := 1;

   -- For loop processes arguments in sequence.
   for I in 1 .. Argument_Count loop
      -- Branch on state.
      case Arg_Processing_State is
         when Waiting_Option =>
            if Argument(I) = "-d" then
               Debug_Mode := True;
            elsif Argument(I) = "-D" then
               Type_Debug_Mode := True;
            elsif Argument(I) = "-a" then
               Animation_Enabled := True;
            elsif Argument(I) = "-e" then
               Arg_Processing_State := Waiting_End_Time;
            elsif Argument(I) = "-r" then
               Arg_Processing_State := Waiting_N_Runs;
            else
               Input_File_Name := To_Unbounded_String(Argument(I));
            end if;
         when Waiting_End_Time =>
            Get(Argument(I), End_Time, Last);
            Arg_Processing_State := Waiting_Option;
         when Waiting_N_Runs =>
            Get(Argument(I), N_Runs, Last);
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
   -- Can't have more than 1 run with no time limit.
   if N_Runs > 1 and End_Time = Real'Last then
      raise Argument_Error with "must provide [-e Time] for more than one run";
   end if;

   -- Read the example file to a string.
   Read_To_String(To_String(Input_File_Name), MIEDS_Code);

   -- Run the parser on it.
   Parse(S          => MIEDS_Code.all,
         Rtn_Tree   => Tree,
         Debug_Mode => Debug_Mode);

   -- Run the type checker on the syntax tree.
   Symbol_Table := new Symbol_Table_Type;
   Type_Check(Tree, Symbol_Table);
   Put_As_XML(Tree);

   -- MUCH OF THE FOLLOWING CODE IS A SHELL FOR FUTURE IMPLEMENTATION
   -- OF THE SIMULATOR.

   -- Do macro expansion.
--     Symbol_Table := new Symbol_Table_Type;
--     Expand(Tree, Symbol_Table, Expanded_Tree);

   -- Print macro expanded tree if in debug mode.
--     if Debug_Mode then
--        Put_As_XML(Expanded_Tree);
--     end if;

   -- Finish the tree: Collapse lambdas and compute cached information.
--     Finish(Expanded_Tree);

--     if Type_Debug_Mode then
--        -- Just print the expanded and finished tree.
--        Put_As_XML(Expanded_Tree);
--     else
--        -- Initialize and run the simulator.
--        Initialize(State, Expanded_Tree);
--        for Run_Number in 1 .. N_Runs loop
--           Put_Line("Run" & Positive'Image(Run_Number));
--           Run(State, End_Time);
--        end loop;
--     end if;

exception
   when Error_Exit =>
      Set_Exit_Status(Failure);
end MIEDS;
