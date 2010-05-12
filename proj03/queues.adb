--                              -*- Mode: Ada -*-
-- Filename        : queues.adb
-- Description     : Simple queue implementation with protected wrapper.
-- Author          : COL Ressler
-- Created On      : Wed Jun 25 21:21:11 2003
-- Last Modified By: COL Ressler
-- Last Modified On: Wed Jun 25 21:21:11 2003
-- Update Count    : 1
-- Status          : Initial implementation.
with Ada.Unchecked_Deallocation;

package body Queues is

   procedure Free is
      new Ada.Unchecked_Deallocation(Node_Type, Node_Ptr_Type);

   procedure Enqueue(Queue : in out Queue_Type;
                     Element : in Element_Type) is
      New_Node : Node_Ptr_Type := new Node_Type'(Element => Element,
                                                 Next => null);
   begin
      if Queue.Tail = null then
         Queue.Head := New_Node;
         Queue.Tail := New_Node;
      else
         Queue.Tail.Next := New_Node;
         Queue.Tail := New_Node;
      end if;
      Queue.N_Nodes := Queue.N_Nodes + 1;
   end Enqueue;

   procedure Dequeue(Queue : in out Queue_Type;
                     Element : out Element_Type) is
      Head : Node_Ptr_Type := Queue.Head;
   begin
      if Head = null then
         raise Empty_Queue_Error;
      end if;
      Element := Head.Element;
      Queue.Head := Head.Next;
      if Queue.Head = null then
         Queue.Tail := null;
      end if;
      Free(Head);
      Queue.N_Nodes := Queue.N_Nodes - 1;
   end Dequeue;

   function Head(Queue : in Queue_Type) return Element_Type is
   begin
      return Queue.Head.Element;
   end Head;

   function Length(Queue : in Queue_Type) return Natural is
   begin
      return Queue.N_Nodes;
   end Length;

   procedure Set_Size_Limit(Queue : in out Queue_Type;
                            Limit : in Natural) is
   begin
      Queue.Size_Limit := Limit;
   end Set_Size_Limit;

   protected body Protected_Queue_Type is

      entry Enqueue(Element : in Element_Type)
      when Length(Queue) < Queue.Size_Limit is
      begin
         Enqueue(Queue, Element);
      end Enqueue;

      entry Dequeue(Element : out Element_Type)
      when Length(Queue) > 0 is
      begin
         Dequeue(Queue, Element);
      end Dequeue;

      function Length return Natural is
      begin
         return Length(Queue);
      end Length;

      procedure Set_Size_Limit(Limit : in Natural) is
      begin
         Set_Size_Limit(Queue, Limit);
      end Set_Size_Limit;

      procedure Clear is
         Dummy_Element : Element_Type;
      begin
         while Length(Queue) > 0 loop
            Dequeue(Queue, Dummy_Element);
         end loop;
      end;

   end Protected_Queue_Type;

end Queues;
