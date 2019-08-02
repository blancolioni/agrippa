with Ada.Strings.Fixed;

with Agrippa.State.Senate;

with Agrippa.Player;

package body Agrippa.UI.Gnoga_UI.Gadgets.Voting is

   --------------------------
   -- Create_Voting_Gadget --
   --------------------------

   procedure Create_Voting_Gadget
     (Gadget : in out Voting_Gadget;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      State  : Agrippa.State.Agrippa_State)
   is
   begin
      Gadget.Create (Parent, "Voting gadget", "voting-gadget");
      Gadget.State := State;
   end Create_Voting_Gadget;

   ----------------------
   -- Current_Business --
   ----------------------

   procedure Current_Business
     (Gadget : in out Voting_Gadget;
      Title  : String)
   is
   begin
      Gadget.Inner_HTML (Title);
   end Current_Business;

   -------------
   -- Propose --
   -------------

   procedure Propose
     (Gadget       : in out Voting_Gadget;
      Senator      : Agrippa.Senators.Agrippa_Senator;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Voting_Order : Agrippa.Factions.Array_Of_Factions)
   is
   begin
      Gadget.Put_Line
        (Senator.Full_Name_And_Faction
         & " proposes " & Proposal.Show);
      Gadget.Voting_Order.Clear;
      for Faction of Voting_Order loop
         Gadget.Voting_Order.Append (Faction);
      end loop;
      Gadget.Next_Vote := 1;
      Gadget.Proposal := Proposal;
      Gadget.Ayes := 0;
      Gadget.Nays := 0;
      Gadget.Voting_Order.Element (Gadget.Next_Vote).Player.Vote (Proposal);
   end Propose;

   ----------
   -- Vote --
   ----------

   procedure Vote
     (Gadget : in out Voting_Gadget;
      Faction : Agrippa.Factions.Agrippa_Faction;
      Aye     : Boolean)
   is
      Votes : constant Natural :=
                Agrippa.State.Senate.Vote_Count (Gadget.State, Faction);
   begin
      Gadget.Put_Line
        (Faction.Name & " votes "
         & (if Aye then "aye (+" else "nay  (-")
         & Ada.Strings.Fixed.Trim (Votes'Image, Ada.Strings.Left)
         & ")");
      if Aye then
         Gadget.Ayes := Gadget.Ayes + Votes;
      else
         Gadget.Nays := Gadget.Nays + Votes;
      end if;

      Gadget.Next_Vote := Gadget.Next_Vote + 1;
      if Gadget.Next_Vote <= Gadget.Voting_Order.Last_Index then
         Gadget.Voting_Order.Element (Gadget.Next_Vote).Player.Vote
           (Gadget.Proposal);
      else
         Gadget.Put_Line
           ("Ayes:" & Gadget.Ayes'Image & "; nays:" & Gadget.Nays'Image);
         if Gadget.Ayes > Gadget.Nays then
            Gadget.Put_Line ("The ayes have it");
         else
            Gadget.Put_Line ("Proposal rejected");
         end if;
         delay 1.0;
         Gadget.State.Change_Handler.On_Proposal_Voted
           (Gadget.Proposal, Gadget.Ayes, Gadget.Nays);
      end if;

   end Vote;

end Agrippa.UI.Gnoga_UI.Gadgets.Voting;
