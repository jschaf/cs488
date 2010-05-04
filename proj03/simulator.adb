-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Text_IO, Ada.Tags, Ada.Numerics.Float_Random;
use  Ada.Text_IO, Ada.Tags, Ada.Numerics.Float_Random;

with Ada.Unchecked_Deallocation;

with Hard_Routes, Hard_Segment_Sets, Animation;
use  Hard_Routes, Hard_Segment_Sets, Animation;

package body Simulator is

   -- Set up these short names for packages to make dot notation more
   -- readable. Can't use "use" because there is no way to tell between
   -- generic Ada.Containers procedures and functions with the same name.
   package EQ  renames Event_Queues;
   package FEL renames Friend_Enroute_Lists;
   package EHL renames Emplaced_Hazard_Lists;

   -- Comparison function for lists of hazards and friends enroute.
   function Is_Same (Left, Right : in Emplaced_Hazard_Type) return Boolean is
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
      Inserted : Boolean; -- never used
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
   
   procedure Friend_Is_Hurt (Friend : in Friend_Enroute_Type; 
                             Hazards : in Emplaced_Hazard_List_Type; 
                             Is_Hurt : out Boolean) is
      Gen : Generator;
      Friend_Vulnerability : Real;
      procedure Maybe_Set_Hurt (Position : EHL.Cursor) is
         Hazard : Emplaced_Hazard_Type := EHL.Element(Position);
         [declarative_item]...
      begin
           if Random(Gen) <= Hazard.Vulnerability * Friend_Vulnerability then
              {statement}...
                [elsif_part]...
                [else_part]
           end if;

         [statement]...
      end Maybe_Set_Hurt;
      [declarative_item]...
   begin
        Find_Friend_Vulnerability(Friend, Friend_Vulnerability);
        
        [statement]...
   end Friend_Is_Hurt;

   -- Handle a friend movement event.
   procedure Handle (Event : in Friend_Movement_Type;
                     State : in out Simulation_State_Type) is
      
         
      -- Native speed of the friend from syntax tree.
      Native_Speed : Real;

      -- Copy the cursor to allow a call of Delete, which changes the cursor.
      Friend_Enroute_Cursor : Friend_Enroute_Cursor_Type := Event.Friend_Enroute_Cursor;

      -- Get the friend enroute referenced by the movement event.
      Friend_Enroute : Friend_Enroute_Type := FEL.Element(Friend_Enroute_Cursor);

      -- True iff friend has completed its route.
      Is_Complete : Boolean;

      -- Id of detonating hazard or 0 if no detonation.
      Hazard_Id : Natural;

      -- Cursor pointing to detonating hazard and a copy of the hazard itself.
      Hazard : Emplaced_Hazard_Type;
      No_Stoppers : Stopper_List_Type(1..0); -- Array of zero elements
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
              Stoppers       => No_Stoppers,
              Stopper_Id     => Hazard_Id);

      -- Check to see if friend completed its route.
      if Is_Complete then
         -- Yes.  Delete the friend enroute from the state.
         FEL.Delete(State.Friends_Enroute, Friend_Enroute_Cursor);
      else
         -- No.  Finish and replace friend enroute data with updated value.
         Friend_Enroute.Last_Update_Time := Event.Time;
         FEL.Replace_Element(State.Friends_Enroute,
                             Friend_Enroute_Cursor,
                             Friend_Enroute);
         -- Schedule the next movement event in the future.
         Schedule_Friend_Movement(State,
                                  Event.Time + Movement_Increment / Native_Speed,
                                  Friend_Enroute_Cursor);
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

         Put_Line("#schedule removal " & Image(Event.Time + Duration));
      else
         EHL.Append(State.Hazards, Emplaced_Hazard);
      end if;

      Find_Threat_Schedule_Interval(Event.Threat, Interval);
      if Duration > 0.0 and Interval > 0.0 then
         Schedule_Hazard(State, Event.Threat, Event.Time + Interval);
      end if;
      Put_Line("@" & Image(Event.Time) &
               " hazard" & Positive'Image(Emplaced_Hazard.Id) &
               " at " & Image(Emplaced_Hazard.Location));
   end Handle;

   -- Handle a hazard removal event.
   procedure Handle(Event : in Hazard_Removal_Type;
                    State : in out Simulation_State_Type) is
      -- Make a copy because Delete parameter is 'in out'.
      C : Ehl.Cursor := Event.Hazard;
   begin
      EHL.Delete(State.Hazards, C);
   end Handle;

   ---------------------------------------------------------------------
   -- Initialization and finalization
   ---------------------------------------------------------------------

   -- Get a simulation state ready to run. (Not much in current version!)
   procedure Initialize(State : out Simulation_State_Type;
                        Model : in Node_Ptr_Type) is
   begin
      State.Model := Model_Ptr_Type(Model);
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
                 Stop_Time : in Real) is
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
   end Run;

end Simulator;
