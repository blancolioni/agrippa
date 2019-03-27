with Ada.Containers.Vectors;

with Agrippa.Messages;
with Agrippa.Proposals;

package body Agrippa.Phases.Senate is

   type Senate_Step is
     (Consul_Nomination, Dictator_Nomination, Censor_Nomination, Open_Session);

   type Senate_Step_Progress is
     (Nomination, Voting);
   pragma Unreferenced (Voting);

   package Voting_Order_Vectors is
     new Ada.Containers.Vectors (Positive, Faction_Id);

   type Senate_Phase_State is
     new Phase_State_Type with
      record
         Step         : Senate_Step;
         Progress     : Senate_Step_Progress;
         Magistrate   : Senator_Id;
         Faction      : Faction_Id;
         Proposal     : Agrippa.Proposals.Proposal_Container_Type;
         Voting_Order : Voting_Order_Vectors.Vector;
      end record;

   type Senate_Phase_Type is
     new Phase_Interface with null record;

   overriding function Name
     (Phase : Senate_Phase_Type)
      return String;

   overriding function Start
     (Phase : Senate_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class;

   overriding function Current_Step_Name
     (Phase       : Senate_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String;

   overriding procedure Step
     (Phase       : Senate_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class);

   -----------------------
   -- Current_Step_Name --
   -----------------------

   overriding function Current_Step_Name
     (Phase       : Senate_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
   is
      Senate_State : Senate_Phase_State renames
                      Senate_Phase_State (Phase_State);
      pragma Unreferenced (Phase);
   begin
      case Senate_State.Step is
         when Consul_Nomination =>
            return State.Local_Text ("consuls");
         when Dictator_Nomination =>
            return State.Local_Text ("dictator");
         when Censor_Nomination =>
            return State.Local_Text ("censor");
         when Open_Session =>
            return State.Local_Text ("open-session");
      end case;
   end Current_Step_Name;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Phase : Senate_Phase_Type)
      return String
   is
      pragma Unreferenced (Phase);
   begin
      return "senate-phase";
   end Name;

   ----------------------
   -- Senate_Phase --
   ----------------------

   function Senate_Phase return Phase_Interface'Class is
   begin
      return Phase : Senate_Phase_Type;
   end Senate_Phase;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Phase : Senate_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class
   is
      pragma Unreferenced (Phase);
   begin
      State.Start_Senate_Session;
      return Phase_State : Senate_Phase_State do
         Phase_State.Step := Consul_Nomination;
         Phase_State.Progress := Nomination;
         Phase_State.Magistrate := State.Highest_Ranking_Available_Officer;
      end return;
   end Start;

   ----------
   -- Step --
   ----------

   overriding procedure Step
     (Phase       : Senate_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is
      pragma Unreferenced (Phase);
      Senate_State : Senate_Phase_State renames
                       Senate_Phase_State (Phase_State);
   begin
      case Senate_State.Step is
         when Consul_Nomination =>
            State.Send_Message
              (Agrippa.Messages.Make_Consular_Nomination
                 (Senate_State.Magistrate,
                  State.Senator_Faction (Senate_State.Magistrate)));

            declare
               Old_Magistrate : constant Senator_Id :=
                                  Senate_State.Magistrate;
            begin
               Senate_State.Magistrate :=
                 State.Highest_Ranking_Available_Officer;

               if Senate_State.Magistrate /= Old_Magistrate then
                  State.Send_Text_Notification
                    (State.Local_Text
                       ("x-is-now-presiding",
                        State.Senator_Name_And_Faction
                          (Senate_State.Magistrate)));
               end if;
            end;

            if State.Crisis then
               Senate_State.Step := Dictator_Nomination;
            else
               Senate_State.Step := Censor_Nomination;
            end if;

         when Dictator_Nomination =>

            State.Send_Message
              (Agrippa.Messages.Make_Office_Nomination
                 (Senate_State.Magistrate,
                  State.Senator_Faction (Senate_State.Magistrate),
                  Dictator));

            declare
               Old_Magistrate : constant Senator_Id :=
                                  Senate_State.Magistrate;
            begin
               Senate_State.Magistrate :=
                 State.Highest_Ranking_Available_Officer;

               if Senate_State.Magistrate /= Old_Magistrate then
                  State.Send_Text_Notification
                    (State.Local_Text
                       ("x-is-now-presiding",
                        State.Senator_Name_And_Faction
                          (Senate_State.Magistrate)));
               end if;
            end;

            Senate_State.Step := Censor_Nomination;

         when Censor_Nomination =>

            State.Send_Message
              (Agrippa.Messages.Make_Office_Nomination
                 (Senate_State.Magistrate,
                  State.Senator_Faction (Senate_State.Magistrate),
                  Censor));

            Senate_State.Step := Open_Session;

         when Open_Session =>

            State.Send_Message
              (Agrippa.Messages.Make_Proposal
                 (Senate_State.Magistrate,
                  State.Senator_Faction (Senate_State.Magistrate),
                  (Agrippa.Proposals.Governor_Nomination,
                   Agrippa.Proposals.Consul_For_Life,
                   Agrippa.Proposals.Recruitment,
                   Agrippa.Proposals.Attack)));

            Senate_State.Finished := State.Senate_Adjourned;

      end case;
   end Step;

end Agrippa.Phases.Senate;
