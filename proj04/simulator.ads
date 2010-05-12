-- Model IED Simulator
-- COL Gene Ressler, 1 December 2007

with Ada.Containers.Ordered_Sets,
     Ada.Containers.Vectors,
     Ada.Containers.Doubly_Linked_Lists;

with Ada.Containers, Ada.Finalization;
use  Ada.Containers, Ada.Finalization;

with Common, AST, AST.Trees;
use  Common, AST, AST.Trees;

package Simulator is

   -- Pointer to any kind of event.  Events are declared later.  We do
   -- this early so the event queue type is available.
   type Event_Type is tagged;
   type Event_Ptr_Type is access all Event_Type'Class;

   -- Set up event queues.
   function Is_Sooner(Left, Right : in Event_Ptr_Type) return Boolean;
   function Is_Same(Left, Right : in Event_Ptr_Type) return Boolean;
   package Event_Queues is
     new Ada.Containers.Ordered_Sets(Event_Ptr_Type, Is_Sooner, Is_Same);
   type Event_Queue_Type is new Event_Queues.Set with null record;
   overriding
   procedure Delete(Container : in out Event_Queue_Type;
                    Position : in out Event_Queues.Cursor);
   type Event_Queue_Ptr_Type is access Event_Queue_Type;

   -- A friend that's currently traveling on a route.
   type Friend_Enroute_Type is record
      Id : Natural := 0;		-- Unique id number.
      Friend : Friend_Ptr_Type;		-- Syntax tree for friend.
      Route : Route_Ptr_Type;		-- Syntax tree for route.
      Position : Real := 0.0;		-- Current linear position along route.
      Location : Point_2d_Type;		-- Current geographic position.
      Segment_Index : Natural := 0;	-- Current segment of route occupied.
      Last_Update_Time : Real := 0.0;	-- Last time a movement event occurred.
   end record;

   -- Set up linked lists of friends enroute.
   function Is_Same(Left, Right : in Friend_Enroute_Type) return Boolean;
   package Friend_Enroute_Lists is
     new Ada.Containers.Doubly_Linked_Lists(Friend_Enroute_Type, Is_Same);
   subtype Friend_Enroute_List_Type is Friend_Enroute_Lists.List;
   subtype Friend_Enroute_Cursor_Type is Friend_Enroute_Lists.Cursor;

   -- A hazard that's currently emplaced.
   type Emplaced_Hazard_Type is record
      Id : Natural := 0;		-- Unique id number.
      Threat : Threat_Ptr_Type;		-- Syntax tree for emplacing threat.
      Location : Point_2d_Type;		-- Geo position of threat.
      -- Cursor to removal event (No_Element => none).
      Removal_Event_Cursor : Event_Queues.Cursor;
   end record;

   -- Set up linked lists of emplaced hazards.
   function Is_Same(Left, Right : in Emplaced_Hazard_Type) return Boolean;
   package Emplaced_Hazard_Lists is
     new Ada.Containers.Doubly_Linked_Lists(Emplaced_Hazard_Type, Is_Same);
   subtype Emplaced_Hazard_List_Type is Emplaced_Hazard_Lists.List;
   subtype Emplaced_Hazard_Cursor_Type is Emplaced_Hazard_Lists.Cursor;
   
   type Completion_Data_Type is record
      Num_Successful_Trips      : Natural := 0;
      Num_Total_Trips           : Natural := 0;
      Num_Successful_Hazards    : Natural := 0;
      Num_Total_Hazards         : Natural := 0;
   end record;
   
   -- Simulation state.
   type Simulation_State_Type is record
      Time : Real := 0.0;  			-- Current simulated time.
      Event_Queue : Event_Queue_Type;		-- Event queue.
      Model : Model_Ptr_Type;  			-- Model we are simulating.
      Hazards : Emplaced_Hazard_List_Type;  	-- Emplaced hazards.
      Friends_Enroute : Friend_Enroute_List_Type; -- Friends enroute.
      Completion_Data : Completion_Data_Type;   -- [un]successful runs
   end record;

   -- Event base type, never instantiated.
   type Event_Type is abstract new Limited_Controlled with record
      Time : Real;
      Id : Natural := 0;
   end record;

   -- Event initializer just ensures unique id number for each event.
   overriding
   procedure Initialize (Object : in out Event_Type);

   procedure Handle(Event : in Event_Type;
                    State : in out Simulation_State_Type) is abstract;

   -- Trip start events.
   type Trip_Start_Type is new Event_Type with record
      Trip : Trip_Ptr_Type;
   end record;
   type Trip_Start_Ptr_Type is access Trip_Start_Type;

   procedure Handle(Event : in Trip_Start_Type;
                    State : in out Simulation_State_Type);

   -- Friend movement events.
   type Friend_Movement_Type is new Event_Type with record
      Friend_Enroute_Cursor : Friend_Enroute_Cursor_Type;
   end record;
   type Friend_Movement_Ptr_Type is access Friend_Movement_Type;

   procedure Handle(Event : in Friend_Movement_Type;
                    State : in out Simulation_State_Type);

   -- Movement events are scheduled to cause roughly this distance of movement.
   Movement_Increment : constant Real := 100.0 / 1000.0; -- 100 meters.

   -- Hazard emplacement events.
   type Hazard_Emplacement_Type is new Event_Type with record
      Threat : Threat_Ptr_Type;
   end record;
   type Hazard_Emplacement_Ptr_Type is access Hazard_Emplacement_Type;

   procedure Handle(Event : in Hazard_Emplacement_Type;
                    State : in out Simulation_State_Type);

   -- Hazard removal events.
   type Hazard_Removal_Type is new Event_Type with record
      Hazard : Emplaced_Hazard_Cursor_Type;
   end record;
   type Hazard_Removal_Ptr_Type is access Hazard_Removal_Type;


   procedure Handle(Event : in Hazard_Removal_Type;
                    State : in out Simulation_State_Type);

   procedure Initialize(State : out Simulation_State_Type;
                        Model : in Node_Ptr_Type);

   -- Run the simulation.
   procedure Run(State : in out Simulation_State_Type;
                 Stop_Time : in Real;
                 Run_Number : in Positive);

end Simulator;
