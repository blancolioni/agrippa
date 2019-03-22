with Agrippa.Messages;

package body Agrippa.Phases.Revolution is

   type Revolution_Phase_State is
     new Phase_State_Type with
      record
         Current_Faction : Faction_Id;
      end record;

   type Revolution_Phase_Type is
     new Phase_Interface with null record;

   overriding function Name
     (Phase : Revolution_Phase_Type)
      return String;

   overriding function Start
     (Phase : Revolution_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class;

   overriding function Current_Step_Name
     (Phase       : Revolution_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String;

   overriding procedure Step
     (Phase       : Revolution_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class);

   -----------------------
   -- Current_Step_Name --
   -----------------------

   overriding function Current_Step_Name
     (Phase       : Revolution_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
   is
      Revolution_State : Revolution_Phase_State renames
                        Revolution_Phase_State (Phase_State);
      pragma Unreferenced (Phase);
   begin
      return State.Faction_Name (Revolution_State.Current_Faction);
   end Current_Step_Name;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Phase : Revolution_Phase_Type)
      return String
   is
      pragma Unreferenced (Phase);
   begin
      return "revolution-phase";
   end Name;

   ----------------------
   -- Revolution_Phase --
   ----------------------

   function Revolution_Phase return Phase_Interface'Class is
   begin
      return Phase : Revolution_Phase_Type;
   end Revolution_Phase;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Phase : Revolution_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class
   is
      pragma Unreferenced (Phase, State);
   begin
      return Phase_State : Revolution_Phase_State do
         Phase_State.Current_Faction := Faction_Id'First;
      end return;
   end Start;

   ----------
   -- Step --
   ----------

   overriding procedure Step
     (Phase       : Revolution_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is
      pragma Unreferenced (Phase);
      Revolution_State : Revolution_Phase_State renames
                      Revolution_Phase_State (Phase_State);
      Faction          : Faction_Id renames
                           Revolution_State.Current_Faction;
      Done             : Boolean := False;

   begin
      for Sid of State.Faction_Senators (Faction) loop
         if State.Get_Senator_State (Sid).Victorious then
            State.Send_Message
              (Agrippa.Messages.Player_Action
                 (Faction, Sid, Agrippa.Messages.Check_Rebellion));
         end if;
      end loop;

      while not Done loop
         declare
            use type Agrippa.Messages.Message_Content_Type;
            Response : constant Agrippa.Messages.Message_Type :=
                         State.Send_Message
                           (Agrippa.Messages.Player_Action
                              (Faction, Agrippa.Messages.Play_Card));
         begin
            Done := Response.Content = Agrippa.Messages.Empty_Message;
         end;
      end loop;

      if Faction = Faction_Id'Last then
         Revolution_State.Finished := True;
      else
         Faction := Faction + 1;
      end if;

   end Step;

end Agrippa.Phases.Revolution;
