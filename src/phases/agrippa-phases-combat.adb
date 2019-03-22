with Ada.Containers.Doubly_Linked_Lists;

with Agrippa.Messages;

package body Agrippa.Phases.Combat is

   type Combat_Phase_Type is
     new Phase_Interface with null record;

   overriding function Name
     (Phase : Combat_Phase_Type)
      return String;

   overriding function Start
     (Phase : Combat_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class;

   overriding function Current_Step_Name
     (Phase       : Combat_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String;

   overriding procedure Step
     (Phase       : Combat_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class);

   package War_Lists is
     new Ada.Containers.Doubly_Linked_Lists (War_Id);

   type Combat_Phase_State_Type is
     new Phase_State_Type with
      record
         Wars : War_Lists.List;
      end record;

   procedure Attack
     (State : in out Agrippa.State.State_Interface'Class;
      War   : War_Id);

   ------------
   -- Attack --
   ------------

   procedure Attack
     (State : in out Agrippa.State.State_Interface'Class;
      War   : War_Id)
   is
   begin
      State.Send_Message (Agrippa.Messages.Attack (War));
   end Attack;

   ----------------------
   -- Combat_Phase --
   ----------------------

   function Combat_Phase return Phase_Interface'Class is
   begin
      return Phase : Combat_Phase_Type;
   end Combat_Phase;

   -----------------------
   -- Current_Step_Name --
   -----------------------

   overriding function Current_Step_Name
     (Phase       : Combat_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
   is
      pragma Unreferenced (Phase, State, Phase_State);
   begin
      return "";
   end Current_Step_Name;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Phase : Combat_Phase_Type)
      return String
   is
      pragma Unreferenced (Phase);
   begin
      return "combat-phase";
   end Name;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Phase : Combat_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class
   is
      pragma Unreferenced (Phase);
      Result : Combat_Phase_State_Type;
      Wars   : constant War_Id_Array :=
                 State.Prosecuted_Wars;
   begin
      for War of Wars loop
         Result.Wars.Append (War);
      end loop;
      return Result;
   end Start;

   ----------
   -- Step --
   ----------

   overriding procedure Step
     (Phase       : Combat_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is
      pragma Unreferenced (Phase);
      Combat_State : Combat_Phase_State_Type renames
                       Combat_Phase_State_Type (Phase_State);
   begin
      if Combat_State.Wars.Is_Empty then
         Phase_State.Finished := True;
         declare
            Wars : constant War_Id_Array := State.Active_Wars;
         begin
            if Wars'Length >= 4 then
               Phase_State.End_Of_Game := True;
               Phase_State.Has_Winner := False;
            end if;
         end;
      else
         Attack (State, Combat_State.Wars.First_Element);
         Combat_State.Wars.Delete_First;
      end if;
   end Step;

end Agrippa.Phases.Combat;
