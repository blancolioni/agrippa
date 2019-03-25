with Ada.Text_IO;

package body Agrippa.State is

   -----------------
   -- Active_Wars --
   -----------------

   function Active_Wars
     (State : State_Interface'Class)
      return War_Id_Array
   is
      function Test (War : War_Id) return Boolean
      is (State.Get_War_State (War).Active);
   begin
      return State.Matching_Wars (Test'Access);
   end Active_Wars;

   -------------------
   -- Inactive_Wars --
   -------------------

   function Inactive_Wars
     (State : State_Interface'Class)
      return War_Id_Array
   is
      function Test (War : War_Id) return Boolean
      is (State.Get_War_State (War).Inactive);
   begin
      return State.Matching_Wars (Test'Access);
   end Inactive_Wars;

   ---------
   -- Log --
   ---------

   procedure Log
     (State   : State_Interface'Class;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (State.Current_Activity
         & ": " & Message);
   end Log;

   ---------------------
   -- Prosecuted_Wars --
   ---------------------

   function Prosecuted_Wars
     (State : State_Interface'Class)
      return War_Id_Array
   is
      function Test (War : War_Id) return Boolean
      is (State.Get_War_State (War).Active
          and then State.Get_War_State (War).Prosecuted);
   begin
      return State.Matching_Wars (Test'Access);
   end Prosecuted_Wars;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (State   : in out State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
   is
      Response : constant Agrippa.Messages.Message_Type :=
                   State.Send_Message (Message);
   begin
      pragma Unreferenced (Response);
   end Send_Message;

   -----------------------
   -- Unprosecuted_Wars --
   -----------------------

   function Unprosecuted_Wars
     (State : State_Interface'Class)
      return War_Id_Array
   is
      function Test (War : War_Id) return Boolean
      is (State.Get_War_State (War).Active
          and then not State.Get_War_State (War).Prosecuted);
   begin
      return State.Matching_Wars (Test'Access);
   end Unprosecuted_Wars;

end Agrippa.State;
