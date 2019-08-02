with Gnoga.Gui.Base;

with Agrippa.State;

private with Agrippa.UI.Gnoga_UI.Gadgets.Pie_Charts;

package Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts is

   type Voting_Chart_Gadget_Type is
     new Agrippa_Gadget_Type with private;

   procedure Create
     (Gadget  : in out Voting_Chart_Gadget_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      State   : not null access Agrippa.State.State_Interface'Class);

private

   type Voting_Chart_Gadget_Type is
     new Pie_Charts.Pie_Chart_Gadget_Type with
      record
         State : access Agrippa.State.State_Interface'Class;
      end record;

end Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts;
