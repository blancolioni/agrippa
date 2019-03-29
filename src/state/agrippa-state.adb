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

   -----------------
   -- Count_Units --
   -----------------

   function Count_Units
     (State : State_Interface'Class;
      Unit  : Unit_Type;
      Test  : not null access
        function (State : Military_Type'Class) return Boolean)
      return Military_Unit_Count
   is
      Count : Military_Unit_Count := 0;
   begin
      for Index in Military_Unit_Index loop
         if Test (State.Military_Unit_State (Unit, Index)) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Units;

   ------------
   -- Crisis --
   ------------

   function Crisis
     (State : State_Interface'Class)
      return Boolean
   is
      Wars      : constant War_Id_Array := State.Active_Wars;
      In_Crisis : Boolean := Wars'Length >= 3;
   begin
      if not In_Crisis then
         for War of Wars loop
            if State.Get_War_State (War).Land_Strength >= 16 then
               In_Crisis := True;
               exit;
            end if;
         end loop;
      end if;
      return In_Crisis;
   end Crisis;

   ---------------------
   -- Deployed_Fleets --
   ---------------------

   function Deployed_Fleets
     (State : State_Interface'Class;
      War   : War_Id)
      return Fleet_Index_Array
   is
      Result : Fleet_Index_Array (1 .. Natural (Fleet_Index'Last));
      Count  : Natural := 0;
   begin
      for Index in Fleet_Index loop
         declare
            Fleet : constant Military_Type'Class :=
                      State.Get_Fleet_State (Index);
         begin
            if Fleet.Created and then Fleet.Deployed
              and then Fleet.War = War
            then
               Count := Count + 1;
               Result (Count) := Index;
            end if;
         end;
      end loop;
      return Result (1 .. Count);
   end Deployed_Fleets;

   ----------------------
   -- Deployed_Legions --
   ----------------------

   function Deployed_Legions
     (State : State_Interface'Class;
      War   : War_Id)
      return Legion_Index_Array
   is
      Result : Legion_Index_Array (1 .. Natural (Legion_Index'Last));
      Count  : Natural := 0;
   begin
      for Index in Legion_Index loop
         declare
            Legion : constant Military_Type'Class :=
                       State.Get_Legion_State (Index);
         begin
            if Legion.Created and then Legion.Deployed
              and then Legion.War = War
            then
               Count := Count + 1;
               Result (Count) := Index;
            end if;
         end;
      end loop;
      return Result (1 .. Count);
   end Deployed_Legions;

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

   ---------------------
   -- Legion_Strength --
   ---------------------

   function Legion_Strength
     (State   : State_Interface'Class;
      Legions : Legion_Index_Array)
      return Combat_Strength
   is
   begin
      return Strength : Combat_Strength := Legions'Length do
         for Index of Legions loop
            if State.Is_Veteran (Index) then
               Strength := Strength + 1;
            end if;
         end loop;
      end return;
   end Legion_Strength;

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
          and then State.Get_War_State (War).Unprosecuted);
   begin
      return State.Matching_Wars (Test'Access);
   end Unprosecuted_Wars;

end Agrippa.State;
