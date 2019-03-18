with Agrippa.Messages;

package body Agrippa.Phases.Revenue is

   type Revenue_Step is
     (Personal_Treasuries,
      State_Treasury,
      Faction_Treasury_Distribution,
      State_Treasury_Contribution);

   type Revenue_Phase_State is
     new Phase_State_Type with
      record
         Step : Revenue_Step;
      end record;

   type Revenue_Phase_Type is
     new Phase_Interface with null record;

   overriding function Name
     (Phase : Revenue_Phase_Type)
      return String;

   overriding function Start
     (Phase : Revenue_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class;

   overriding function Current_Step_Name
     (Phase       : Revenue_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String;

   overriding procedure Step
     (Phase       : Revenue_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class);

   -----------------------
   -- Current_Step_Name --
   -----------------------

   overriding function Current_Step_Name
     (Phase       : Revenue_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
   is
      pragma Unreferenced (State);
      Revenue_State : Revenue_Phase_State renames
                        Revenue_Phase_State (Phase_State);
      pragma Unreferenced (Phase);
   begin
      case Revenue_State.Step is
         when Personal_Treasuries =>
            return "Personal Treasuries";
         when State_Treasury =>
            return "State Treasury";
         when Faction_Treasury_Distribution =>
            return "Faction Transfers";
         when State_Treasury_Contribution =>
            return "Contributions to State Treasury";
      end case;
   end Current_Step_Name;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Phase : Revenue_Phase_Type)
      return String
   is
      pragma Unreferenced (Phase);
   begin
      return "revenue-phase";
   end Name;

   -------------------
   -- Revenue_Phase --
   -------------------

   function Revenue_Phase return Phase_Interface'Class is
   begin
      return Phase : Revenue_Phase_Type;
   end Revenue_Phase;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Phase : Revenue_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class
   is
      pragma Unreferenced (Phase, State);
   begin
      return Phase_State : constant Revenue_Phase_State := Revenue_Phase_State'
        (Finished => False, Step => Personal_Treasuries, others => <>);
   end Start;

   ----------
   -- Step --
   ----------

   overriding procedure Step
     (Phase       : Revenue_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is
      pragma Unreferenced (Phase);
      Revenue_State : Revenue_Phase_State renames
                        Revenue_Phase_State (Phase_State);
   begin
      case Revenue_State.Step is
         when Personal_Treasuries =>
            State.Evaluate_Faction_Revenue;
         when State_Treasury =>
            State.Evaluate_State_Revenue;
         when Faction_Treasury_Distribution =>
            for Faction in Faction_Id loop
               State.Send_Message
                 (Agrippa.Messages.Faction_Transfers (Faction));
            end loop;
         when State_Treasury_Contribution =>
            null;
      end case;

      if Revenue_State.Step = Revenue_Step'Last then
         Revenue_State.Finished := True;
      else
         Revenue_State.Step := Revenue_Step'Succ (Revenue_State.Step);
      end if;

   end Step;

end Agrippa.Phases.Revenue;
