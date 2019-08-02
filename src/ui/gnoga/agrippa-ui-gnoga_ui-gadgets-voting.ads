private with Ada.Containers.Vectors;

with Agrippa.Factions;
with Agrippa.Senators;

with Agrippa.Proposals;

package Agrippa.UI.Gnoga_UI.Gadgets.Voting is

   type Voting_Gadget is
     new Agrippa_Gadget_Type with private;

   procedure Create_Voting_Gadget
     (Gadget : in out Voting_Gadget;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      State  : Agrippa.State.Agrippa_State);

   procedure Current_Business
     (Gadget : in out Voting_Gadget;
      Title  : String);

   procedure Propose
     (Gadget       : in out Voting_Gadget;
      Senator      : Agrippa.Senators.Agrippa_Senator;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Voting_Order : Agrippa.Factions.Array_Of_Factions);

   procedure Vote
     (Gadget : in out Voting_Gadget;
      Faction : Agrippa.Factions.Agrippa_Faction;
      Aye     : Boolean);

private

   package Voting_Order_Vectors is
     new Ada.Containers.Vectors
       (Positive, Agrippa.Factions.Agrippa_Faction, Agrippa.Factions."=");

   type Voting_Gadget is
     new Agrippa_Gadget_Type with
      record
         State        : Agrippa.State.Agrippa_State;
         Proposal     : Agrippa.Proposals.Agrippa_Proposal;
         Voting_Order : Voting_Order_Vectors.Vector;
         Next_Vote    : Natural := 0;
         Ayes         : Natural := 0;
         Nays         : Natural := 0;
      end record;

end Agrippa.UI.Gnoga_UI.Gadgets.Voting;
