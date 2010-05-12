--                              -*- Mode: Ada -*-
-- Filename        : queues.ads
-- Description     : Specification for simple queues with protected wrapper.
-- Author          : COL Ressler
-- Created On      : Wed Jun 25 21:22:14 2003
-- Last Modified By: COL Ressler
-- Last Modified On: Wed Jun 25 21:22:14 2003
-- Update Count    : 1
-- Status          : Initial implementation.
generic
   type Element_Type is private;
package Queues is

   type Queue_Type is private;

   procedure Enqueue(Queue : in out Queue_Type;
                     Element : in Element_Type);

   procedure Dequeue(Queue : in out Queue_Type;
                     Element : out Element_Type);

   function Head(Queue : in Queue_Type) return Element_Type;

   function Length(Queue : in Queue_Type) return Natural;

   procedure Set_Size_Limit(Queue : in out Queue_Type;
                            Limit : in Natural);

   Empty_Queue_Error : exception;

   protected type Protected_Queue_Type is

      entry Enqueue(Element : in Element_Type);

      entry Dequeue(Element : out Element_Type);

      function Length return Natural;

      procedure Set_Size_Limit(Limit : in Natural);

      procedure Clear;

   private
      Queue : Queue_Type;
   end Protected_Queue_Type;

private

   type Node_Type;

   type Node_Ptr_Type is access Node_Type;

   type Node_Type is
      record
         Element : Element_Type;
         Next : Node_Ptr_Type;
      end record;

   type Queue_Type is
      record
         Head, Tail : Node_Ptr_Type;
         Size_Limit : Natural := Natural'Last;
         N_Nodes : Natural := 0;
      end record;

end Queues;
