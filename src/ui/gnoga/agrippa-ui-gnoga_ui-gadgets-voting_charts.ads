with Gnoga.Gui.Base;

private with Agrippa.UI.Gnoga_UI.Gadgets.Pie_Charts;

package Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts is

   type Voting_Chart_Gadget_Type is
     new Agrippa_Gadget_Type with private;

   procedure Create
     (Gadget  : in out Voting_Chart_Gadget_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      State   : Agrippa.State.Agrippa_State);

private

   type Voting_Chart_Gadget_Type is
     new Pie_Charts.Pie_Chart_Gadget_Type with
      record
         State : Agrippa.State.Agrippa_State;
      end record;

end Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts;
