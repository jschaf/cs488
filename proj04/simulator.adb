-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Long_Float_Text_IO, Ada.Tags, Ada.Numerics.Float_Random;
use  Ada.Text_IO, Ada.Long_Float_Text_IO, Ada.Tags, Ada.Numerics.Float_Random;

with Ada.Unchecked_Deallocation;

with Hard_Routes, Hard_Segment_Sets, Animation;
use  Hard_Routes, Hard_Segment_Sets, Animation;

package body Simulator is

   -- Set up these short names for packages to make dot notation more
   -- readable. Can't use "use" because there is no way to tell between
   -- procedures and functions with the same name.
   package EQ  renames Event_Queues;
   package FEL renames Friend_Enroute_Lists;
   package EHL renames Emplaced_Hazard_Lists;


   -- Comparison function for lists of hazards and friends enroute.
   function Is_Same(Left, Right : in Emplaced_Hazard_Type) return Boolean is
   begin
      return Left.Id = Right.Id;
   end Is_Same;

   function Is_Same (Left, Right : in Friend_Enroute_Type) return Boolean is
   begin
      return Left.Id = Right.Id;
   end Is_Same;

   -- Override controlled base type to ensure every event gets its
   -- own unique id automatically.
   Next_Event_Id : Positive := 1;
   overriding
   procedure Initialize (Object : in out Event_Type) is
   begin
      Object.Id := Next_Event_Id;
      Next_Event_Id := Next_Event_Id + 1;
   end Initialize;

   -- Since our events are allocated with 'new' when we queue them up,
   -- we must deallocate them after removal.  You'll see this below.
   -- A run may need millions of events.  Must reuse their storage.
   procedure Free is
     new Ada.Unchecked_Deallocation (Event_Type'Class, Event_Ptr_Type);

   -- Comparison functions for event queues (just like barber shop simulation).
   function Is_Sooner (Left, Right : in Event_Ptr_Type) return Boolean is
   begin
      return Left.Time < Right.Time or
        (Left.Time = Right.Time and Left.Id < Right.Id);
   end Is_Sooner;

   function Is_Same (Left, Right : in Event_Ptr_Type) return Boolean is
   begin
      return Left.Id = Right.Id;
   end Is_Same;

   -- Specialize the behavior of Delete so that the allocated event is freed.
   overriding
   procedure Delete(Container : in out Event_Queue_Type;
                    Position : in out EQ.Cursor) is
      Event : Event_Ptr_Type := EQ.Element(Position);
   begin
      Free(Event);
      EQ.Delete(EQ.Set(Container), Position);
   end Delete;

   ---------------------------------------------------------------------
   -- Event schedulers
   ---------------------------------------------------------------------

   -- Add a trip start to the event queue.
   procedure Schedule_Trip_Start(State : in out Simulation_State_Type;
                                 Trip : in Trip_Ptr_Type;
                                 Time : in Real) is
      Event : Trip_Start_Ptr_Type;
   begin
      Event := new Trip_Start_Type;
      Event.Time := Time;
      Event.Trip := Trip;
      Insert(State.Event_Queue, Event_Ptr_Type(Event));
   end Schedule_Trip_Start;

   -- Add a hazard emplacement event to the event queue.
   procedure Schedule_Hazard(State : in out Simulation_State_Type;
                             Threat : in Threat_Ptr_Type;
                             Time : in Real) is
      Event : Hazard_Emplacement_Ptr_Type;
   begin
      Event := new Hazard_Emplacement_Type;
      Event.Time := Time;
      Event.Threat := Threat;
      Insert(State.Event_Queue, Event_Ptr_Type(Event));
   end Schedule_Hazard;

   -- Add a hazard removal event to the event queue.
   procedure Schedule_Hazard_Removal(State : in out Simulation_State_Type;
                                     Hazard : in Emplaced_Hazard_Cursor_Type;
                                     Time : in Real;
                                     Event_Cursor : out EQ.Cursor) is
      Event : Hazard_Removal_Ptr_Type;
      Inserted : Boolean;
   begin
      Event := new Hazard_Removal_Type;
      Event.Time := Time;
      Event.Hazard := Hazard;
      Insert(State.Event_Queue,
             Event_Ptr_Type(Event),
             Event_Cursor,
             Inserted);
   end Schedule_Hazard_Removal;

   -- Add a friend movement to the event queue.
   procedure Schedule_Friend_Movement
     (State : in out Simulation_State_Type;
      Time : in Real;
      Friend_Enroute : in Friend_Enroute_Cursor_Type)
   is
      Event : Friend_Movement_Ptr_Type;
   begin
      Event := new Friend_Movement_Type;
      Event.Time := Time;
      Event.Friend_Enroute_Cursor := Friend_Enroute;
      Insert(State.Event_Queue, Event_Ptr_Type(Event));
   end Schedule_Friend_Movement;

   ---------------------------------------------------------------------
   -- Event handlers
   ---------------------------------------------------------------------

   -- Handle a trip start event.
   Next_Friend_Enroute_Id : Positive := 1;
   procedure Handle (Event : in Trip_Start_Type;
                     State : in out Simulation_State_Type) is
      use Hard_Route_Vectors;
      Route : Route_Ptr_Type := Route_Ptr_Type(Event.Trip.Route);
      Interval : Real;
   begin
      -- Add a friend enroute to make this trip.
      FEL.Append(State.Friends_Enroute,
        Friend_Enroute_Type'(Id => Next_Friend_Enroute_Id,
                             Friend => Friend_Ptr_Type(Event.Trip.Friend),
                             Route => Route,
                             Position => 0.0, -- Start of route.
                             Segment_Index => 1, -- First segment.
                             Location => First_Element(Route.Hard_Route).Point,
                             Last_Update_Time => Event.Time));
      Next_Friend_Enroute_Id := Next_Friend_Enroute_Id + 1;

      -- Schedule movement to the start point.
      Schedule_Friend_Movement(State,
                               Event.Time,
                               FEL.Last(State.Friends_Enroute));

      -- Schedule the next trip start.
      Find_Trip_Schedule_Interval(Event.Trip, Interval);
      if Interval > 0.0 then
         Schedule_Trip_Start(State,
                             Event.Trip,
                             Event.Time + Interval);
      end if;
   end Handle;


   -- Random determination of whether a friend is hurt by a hazard detonation.
   Detonation_Random : Generator;
   function Friend_Is_Hurt(Friend_Enroute : in Friend_Enroute_Type;
                           Hazard : in Emplaced_Hazard_Type) return Boolean is
   begin
      return
        Real(Random(Detonation_Random)) <=
          Hazard.Threat.Hard_Effectiveness *
            Friend_Enroute.Friend.Hard_Vulnerability;
   end Friend_Is_Hurt;

   -- Translate the current list of hazards into a stopper list for the
   -- Advance procedure in Hard_Routes.
   function Stopper_List(State : in Simulation_State_Type)
                         return Stopper_List_Type is
      Hazard : Emplaced_Hazard_Type;
      C : EHL.Cursor;
   begin
      return List : Stopper_List_Type(1 .. Natural(EHL.Length(State.Hazards))) do
         C := EHL.First(State.Hazards);
         for I in List'Range loop
            Hazard := EHL.Element(C);
            List(I) := (Id => Hazard.Id, Location => Hazard.Location);
         end loop;
      end return;
   end Stopper_List;
   
   procedure Increment (I : in out Integer) is
   begin
      I := I + 1;
   end Increment;
      
   procedure Log_Successful_Trip (State : in out simulation_state_type) is
      Data : Completion_Data_Type renames State.Completion_Data;
   begin
      Increment(Data.Num_Successful_Trips);
      Increment(Data.Num_Total_Trips);
   end Log_Successful_Trip;
   
   procedure Log_Unsuccessful_Trip (State : in out Simulation_State_Type) is
      Data : Completion_Data_Type renames State.Completion_Data;
   begin
      Increment(Data.Num_Total_Trips);
      Increment(Data.Num_Successful_Hazards);
   end Log_Unsuccessful_Trip;
   
   procedure Log_Hazard_Emplacement (State : in out Simulation_State_Type) is
      Data : Completion_Data_Type renames State.Completion_Data;
   begin
      Increment(Data.Num_Total_Hazards);
   end Log_Hazard_Emplacement;
   
   -- Handle a friend movement event.
   procedure Handle (Event : in Friend_Movement_Type;
                     State : in out Simulation_State_Type) is

      use type EQ.Cursor; -- allows comparison of cursors with /=

      -- Native speed of the friend from syntax tree.
      Native_Speed : Real;

      -- Copy the cursor to allow a call of Delete, which changes the cursor.
      Friend_Enroute_Cursor : Friend_Enroute_Cursor_Type := Event.Friend_Enroute_Cursor;

      -- Get the friend enroute referenced by the movement event.
      Friend_Enroute : Friend_Enroute_Type := FEL.Element(Friend_Enroute_Cursor);

      -- Update the moving friend enroute and schedule the next movement event.
      -- This is here just to prevent repeating the code twice below.  It makes
      -- use of the variables above.
      procedure Continue_Movement is
      begin
         -- No.  Finish and replace friend enroute data with updated value.
         Friend_Enroute.Last_Update_Time := Event.Time;
         FEL.Replace_Element(State.Friends_Enroute,
                             Friend_Enroute_Cursor,
                             Friend_Enroute);
         -- Schedule the next movement event in the future.
         Schedule_Friend_Movement(State,
                                  Event.Time + Movement_Increment / Native_Speed,
                                  Friend_Enroute_Cursor);
      end Continue_Movement;

      -- True iff friend has completed its route.
      Is_Complete : Boolean;

      -- Id of detonating hazard or 0 if no detonation.
      Hazard_Id : Natural;

      -- Cursor pointing to detonating hazard and a copy of the hazard itself.
      Hazard_Cursor : Emplaced_Hazard_Cursor_Type;
      Hazard : Emplaced_Hazard_Type;
   begin
      -- Evaluate the expression for friend speed.
      Find_Friend_Speed(Friend_Enroute.Friend, Native_Speed);

      -- Advance the friend along its route using cached hard route data.
      Advance(Route          => Friend_Enroute.Route.Hard_Route,
              Native_Speed   => Native_Speed,
              Travel_Time    => Event.Time - Friend_Enroute.Last_Update_Time,
              Segment_Index  => Friend_Enroute.Segment_Index,
              Position       => Friend_Enroute.Position,
              Point          => Friend_Enroute.Location,
              Is_Complete    => Is_Complete,
              Stopper_Radius => 10.0 / 1000.0, -- 10 meters
              Stoppers       => Stopper_List(State),
              Stopper_Id     => Hazard_Id);

        --Put_Line("@" & Image(Event.Time) &
         --        " advance" & Positive'Image(Friend_Enroute.Id) &
         --        " to " & Image(Friend_Enroute.Location) &
         --        "(" & Image(Friend_Enroute.Position) & ")");

      -- Handle hazard detonation, if any.
      if Hazard_Id > 0 then


         -- Find the hazard using the id returned from Advance.
         Hazard_Cursor := EHL.Find(State.Hazards, (Id => Hazard_Id, others => <>));
         Hazard := EHL.Element(Hazard_Cursor);

         -- Do random determination if friend is hurt by explosion.
         if Friend_Is_Hurt(Friend_Enroute, Hazard) then
            FEL.Delete(State.Friends_Enroute, Friend_Enroute_Cursor);
            Log_Unsuccessful_Trip(State);
            --  put_line("Friend hurt :(");
         else
            --  put_line("Friend safe :)");
            Continue_Movement;
         end if;

         -- If the hazard was going to be removed in the future, dequeue the
         -- event that will do this. You can't remove the hazard after it's
         -- already been removed here.
         if Hazard.Removal_Event_Cursor /= EQ.No_Element then
            Delete(State.Event_Queue, Hazard.Removal_Event_Cursor);
         end if;

         -- Delete the hazard that has detonated.
         Emplaced_Hazard_Lists.Delete(State.Hazards, Hazard_Cursor);
         
      -- Check to see if friend completed its route.
      elsif Is_Complete then
         -- Yes.  Delete the friend enroute from the state.
         FEL.Delete(State.Friends_Enroute, Friend_Enroute_Cursor);
         Log_Successful_Trip(State);
      else
         Continue_Movement;
      end if;
   end Handle;

   -- Handle a hazard emplacement event.
   Next_Hazard_Id : Positive := 1;
   procedure Handle (Event : in Hazard_Emplacement_Type;
                     State : in out Simulation_State_Type) is
      Duration, Interval : Real;
      Emplaced_Hazard : Emplaced_Hazard_Type;
   begin
      -- Find out how long the hazard is going to be present before removal.
      Find_Threat_Duration(Event.Threat, Duration);
      --put_line("threat duration  = "&Real'Image(Duration));
      
      Log_Hazard_Emplacement(State);
      
      -- Begin building the hazard.
      Emplaced_Hazard.Id := Next_Hazard_Id;
      Next_Hazard_Id := Next_Hazard_Id + 1;
      Emplaced_Hazard.Threat := Event.Threat;
      Get_Random_Point(Event.Threat.Segment_Set, Emplaced_Hazard.Location);

      -- If the hazard is going to be removed, we still need to set the
      -- Removal_Event_Cursor field.  This happens below.

      if Duration > 0.0 then
         -- Append an empty hazard to the list so that Last() points to it.
         EHL.Append(State.Hazards, Emplaced_Hazard_Type'(others => <>));

         -- Schedule the removal event the empty hazard and get a cursor to that
         -- event as a return value to complete the hazard we are constructing.
         Schedule_Hazard_Removal(State,
                                 EHL.Last(State.Hazards),
                                 Event.Time + Duration,
                                 Emplaced_Hazard.Removal_Event_Cursor);

         -- Now replace the empty hazard record with the real thing.
         EHL.Replace_Element(State.Hazards,
                             EHL.Last(State.Hazards),
                             Emplaced_Hazard);

      else
         EHL.Append(State.Hazards, Emplaced_Hazard);
      end if;

      Find_Threat_Schedule_Interval(Event.Threat, Interval);

      if Duration > 0.0 and Interval > 0.0 then
         Schedule_Hazard(State, Event.Threat, Event.Time + Interval);
      end if;

   end Handle;

   -- Handle a hazard removal event.
   procedure Handle(Event : in Hazard_Removal_Type;
                    State : in out Simulation_State_Type) is
      -- Make a copy because Delete parameter is 'in out'.
      C : Ehl.Cursor := Event.Hazard;
      Hazard : Emplaced_Hazard_Type;
   begin
      Hazard := EHL.Element(C);
      EHL.Delete(State.Hazards, C);
   end Handle;

   ---------------------------------------------------------------------
   -- Initialization and finalization
   ---------------------------------------------------------------------

   -- Get a simulation state ready to run. (Not much in current version!)
   detonate_seed : Positive := 42;

   procedure Initialize(State : out Simulation_State_Type;
                        Model : in Node_Ptr_Type) is
   begin
      State.Time := 0.0;
      State.Model := Model_Ptr_Type(Model);
      clear(State.Event_Queue);		-- Event queue.
      EHL.clear(State.Hazards);  	-- Emplaced hazards.
      FEL.clear(State.Friends_Enroute); -- Friends enroute.

      -- reset locations
      Initialize_Segments;

      Reset(Detonation_Random, detonate_seed); -- Always get same values.
      detonate_seed := detonate_seed +1;
   end Initialize;

   -- Loop through all the instances in the model and schedule the
   -- initial trip start and hazard emplacement events.
   procedure Schedule_Initial_Events(State : in out Simulation_State_Type) is
      use Node_Lists;
      C : Cursor;
      Instance : Instance_Ptr_Type;
      Start_Time : Real;
      Trip : Trip_Ptr_Type;
      Threat : Threat_Ptr_Type;
   begin
      C := First(State.Model.Instances);
      while C /= No_Element loop
         Instance := Instance_Ptr_Type(Element(C));

         -- Instances are either trips or threats.  Handle both cases.
         if Instance.Description.all in Trip_Type then
            Trip := Trip_Ptr_Type(Instance.Description);
            -- Compute start time from instance and schedule trip start.
            Find_Trip_Schedule_Start(Trip, Start_Time);
            Schedule_Trip_Start(State, Trip, Start_Time);
         else -- Must be Threat_Type.
            -- Description must be a threat!
            Threat := Threat_Ptr_Type(Instance.Description);
            Find_Threat_Schedule_Start(Threat, Start_Time);
            -- Schedule first hazard emplacement
            Schedule_Hazard(State, Threat, Start_Time);
         end if;
         C := Next(C);
      end loop;
   end Schedule_Initial_Events;
   
   function Calc_Trip_Success_Rate (State : in Simulation_State_Type) 
                                   return Real is
      Data : Completion_Data_Type renames State.Completion_Data;
   begin
      return Real(Data.Num_Successful_Trips) / Real(Data.Num_Total_Trips);
   end Calc_Trip_Success_Rate;
   
   function Calc_Hazard_Success_Rate (State : in Simulation_State_Type) 
                                     return Real is
      Data : Completion_Data_Type renames State.Completion_Data;
   begin
      return Real(Data.Num_Successful_Hazards) / Real(Data.Num_Total_Hazards);
   end Calc_Hazard_Success_Rate;
   
   procedure Print_Real (R : in Real) is
   begin
      Put(Item => R, Aft => 4, Exp => 0);
   end Print_Real;
   ---------------------------------------------------------------------
   -- Simulation loop
   ---------------------------------------------------------------------

   -- This pops the next event off the front of the event queue.  Because
   -- we've used an ordered set from Ada.Containers, the first element is
   -- always the one with the smallest time.
   procedure Get_Next_Event (State : in out Simulation_State_Type;
                             Event : out Event_Ptr_Type) is
   begin
      Event := First_Element (State.Event_Queue);
      Delete_First (State.Event_Queue);
   end Get_Next_Event;

   -- Run the simulation event loop up to given stop time.
   procedure Run(State : in out Simulation_State_Type;
                 Stop_Time : in Real;
                 Run_Number : in Positive) is
      Event : Event_Ptr_Type;
   begin
      -- Initialize the background image of the animation one time.
      Animation.Initialize(State);

      -- Place the initial events on the event queue.
      Schedule_Initial_Events(State);

      -- Process events until the stop time so long as event queue has events
      -- left to process.
      while State.Time < Stop_Time and not Is_Empty(State.Event_Queue) loop

         -- Get an event from the queue.
         Get_Next_Event(State, Event);

         -- Update the simulation clock from the event.
         State.Time := Event.Time;

         -- Call the handler to process the event.
         Handle(Event.all, State);

         -- Free storage for the event. We're done with it.
         Free(Event);

         -- Redraw the animation.
         Animation.Update(State);
      end loop;
      -- Print data in a pseudo CSV format
      Put("Run" & Positive'image(Run_Number) & ",");
      Print_Real(Calc_Trip_Success_Rate(State));
      Put(",");
      Print_Real(Calc_Hazard_Success_Rate(State));
      New_Line;
   end Run;

end Simulator;
