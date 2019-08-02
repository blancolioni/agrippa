private with Ada.Containers.Vectors;
private with Gnoga.Gui.Element.Common;

with Gnoga.Gui.Base;
with Gnoga.Gui.Element;

with Agrippa.Factions;
with Agrippa.Senators;

with Agrippa.Proposals;

package Agrippa.UI.Gnoga_UI.Phase_Views is

   type Agrippa_Phase_View_Type is
     abstract new Gnoga.Gui.Element.Element_Type with private;

   type Phase_View is access all Agrippa_Phase_View_Type'Class;

   function Phase_Name
     (View : Agrippa_Phase_View_Type)
      return String
      is abstract;

   procedure Create_Phase_View
     (View    : in out Agrippa_Phase_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class)
   is abstract;

   procedure Activate
     (View    : in out Agrippa_Phase_View_Type)
   is abstract;

   procedure Vote
     (View    : in out Agrippa_Phase_View_Type;
      Faction : Agrippa.Factions.Agrippa_Faction;
      Aye     : Boolean);

   procedure Propose
     (View         : in out Agrippa_Phase_View_Type;
      Sponsor      : Agrippa.Senators.Agrippa_Senator;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Voting_Order : Agrippa.Factions.Array_Of_Factions);

   procedure Voting_Complete
     (View         : in out Agrippa_Phase_View_Type;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Ayes, Nays   : Natural);

   type Phase_View_Container is
     new Gnoga.Gui.Element.Element_Type with private;

   procedure Create
     (Container : in out Phase_View_Container;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      State     : Agrippa.State.Agrippa_State);

   function Current_Phase
     (Container : Phase_View_Container'Class)
      return Phase_View;

   procedure Next_Phase
     (Container : in out Phase_View_Container'Class);

private

   type Agrippa_Phase_View_Type is
     abstract new Gnoga.Gui.Element.Common.DIV_Type with
      record
         State : Agrippa.State.Agrippa_State;
      end record;

   package Phase_View_Vectors is
     new Ada.Containers.Vectors (Positive, Phase_View);

   type Phase_View_Container is
     new Gnoga.Gui.Element.Common.DIV_Type with
      record
         Views         : Phase_View_Vectors.Vector;
         Current_Index : Natural := 0;
      end record;

   function Current_Phase
     (Container : Phase_View_Container'Class)
      return Phase_View
   is (Container.Views.Element (Container.Current_Index));

end Agrippa.UI.Gnoga_UI.Phase_Views;
