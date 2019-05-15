with Ada.Strings.Unbounded;

with Agrippa.Factions;
with Agrippa.Offices;
with Agrippa.Senators;

with Agrippa.Player;

with Agrippa.State.Senate;

with Agrippa.UI.Gnoga_UI.Gadgets.Voting;

package body Agrippa.UI.Gnoga_UI.Phase_Views.Senate is

   type Senate_Business is (Holding_Election);

   type Senate_Phase_Type is
     new Agrippa_Phase_View_Type with
      record
         Current_Business : Senate_Business;
         Vote             : Agrippa.UI.Gnoga_UI.Gadgets.Voting.Voting_Gadget;
      end record;

   overriding function Phase_Name
     (Phase : Senate_Phase_Type)
      return String
   is ("senate-phase");

   overriding procedure Create_Phase_View
     (View    : in out Senate_Phase_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class);

   overriding procedure Activate
     (View    : in out Senate_Phase_Type);

   overriding procedure Propose
     (View         : in out Senate_Phase_Type;
      Sponsor      : Agrippa.Senators.Agrippa_Senator;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Voting_Order : Agrippa.Factions.Array_Of_Factions);

   overriding procedure Vote
     (View    : in out Senate_Phase_Type;
      Faction : Agrippa.Factions.Agrippa_Faction;
      Aye     : Boolean);

   overriding procedure Voting_Complete
     (View         : in out Senate_Phase_Type;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Ayes, Nays   : Natural);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (View    : in out Senate_Phase_Type)
   is
      use Ada.Strings.Unbounded;
      State       : constant Agrippa.State.Agrippa_State := View.State;
      HRO         : constant Agrippa.Senators.Agrippa_Senator :=
                      Agrippa.State.Senate.Highest_Ranking_Available_Officer
                        (State);
      Vacancies   : constant Agrippa.Offices.Array_Of_Offices :=
                      (Agrippa.Offices.President,
                       Agrippa.Offices.Admiral,
                       Agrippa.Offices.General);
      Business    : Unbounded_String :=
                      To_Unbounded_String ("Holding election for vacancies:");
   begin
      for Office of Vacancies loop
         Business := Business & " "
           & Agrippa.Offices.Office_Name (Office);
      end loop;
      View.Current_Business := Holding_Election;
      View.Vote.Current_Business (To_String (Business));

      HRO.Faction.Player.Create_Election_Proposal
        (Vacancies, HRO);

--        View.Put_Line
--          (HRO.Full_Name_And_Faction
--           & " proposes " & Proposal.Show);
--
--        declare
--           Ayes, Nays : Natural := 0;
--
--        procedure Show_Response (Faction : Agrippa.Factions.Agrippa_Faction);
--
--           ------------------
--           -- Show_Reponse --
--           ------------------
--
--           procedure Show_Response
--             (Faction : Agrippa.Factions.Agrippa_Faction)
--           is
--              Score : constant Integer :=
--                        Proposal.Evaluate (State, Faction);
--              Votes : constant Natural :=
--                        Agrippa.State.Senate.Vote_Count (State, Faction);
--           begin
--              View.Put_Line
--                (Faction.Identifier & " scores proposal: " & Score'Image);
--              if Proposal.Proposed_By_Faction (Faction)
--                or else Score >= 0
--              then
--                 Ayes := Ayes + Votes;
--              else
--                 Nays := Nays + Votes;
--              end if;
--
--           end Show_Response;
--
--        begin
--           for I in 1 .. State.Factions.Count loop
--              Show_Response (State.Factions.Get (I));
--           end loop;
--
--           View.Put_Line ("Ayes:" & Ayes'Img);
--           View.Put_Line ("Nays:" & Nays'Img);
--           View.Put_Line
--             ("The " & (if Ayes >= Nays then "ayes" else "nays")
--              & " have it");
--           if Ayes >= Nays then
--              Proposal.Enact (State);
--           end if;
--        end;

   end Activate;

   -----------------------
   -- Create_Phase_View --
   -----------------------

   overriding procedure Create_Phase_View
     (View    : in out Senate_Phase_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      View.Create (Parent);
      View.Vote.Create_Voting_Gadget (View, View.State);
   end Create_Phase_View;

   -------------
   -- Propose --
   -------------

   overriding procedure Propose
     (View         : in out Senate_Phase_Type;
      Sponsor      : Agrippa.Senators.Agrippa_Senator;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Voting_Order : Agrippa.Factions.Array_Of_Factions)
   is
   begin
      View.Vote.Propose (Sponsor, Proposal, Voting_Order);
   end Propose;

   ------------------
   -- Senate_Phase --
   ------------------

   function Senate_Phase
     (State : Agrippa.State.Agrippa_State)
      return Phase_View
   is
   begin
      return Phase : constant Phase_View := new Senate_Phase_Type do
         Phase.State := State;
      end return;
   end Senate_Phase;

   ----------
   -- Vote --
   ----------

   overriding procedure Vote
     (View    : in out Senate_Phase_Type;
      Faction : Agrippa.Factions.Agrippa_Faction;
      Aye     : Boolean)
   is
   begin
      View.Vote.Vote (Faction, Aye);
   end Vote;

   overriding procedure Voting_Complete
     (View         : in out Senate_Phase_Type;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Ayes, Nays   : Natural)
   is
   begin
      View.Vote.Current_Business ("Senate");
      if Ayes > Nays then
         Proposal.Enact (View.State);
      end if;
   end Voting_Complete;

end Agrippa.UI.Gnoga_UI.Phase_Views.Senate;
