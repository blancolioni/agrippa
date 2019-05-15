private with WL.Guids;

with Gnoga.Types;
with Gnoga.Gui.Window;

private with Gnoga.Gui.Element.Common;
private with Gnoga.Gui.Element.Form;
private with Gnoga.Gui.View;

with Agrippa.State;
private with Agrippa.State.Notifications;

with Agrippa.UI.Gnoga_UI.Views;

private with Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts;

package Agrippa.Sessions is

   type Root_Agrippa_Session is
     new Gnoga.Types.Connection_Data_Type with private;

   type Agrippa_Session is access all Root_Agrippa_Session'Class;

   function New_Session
     (Main_Window : Gnoga.Gui.Window.Pointer_To_Window_Class)
      return Agrippa_Session;

   procedure Close_Session (Session : in out Agrippa_Session);

private

   type Faction_Status_Record is
      record
         Faction_View : Agrippa.UI.Gnoga_UI.Views.Agrippa_View_Type;
      end record;

   type Faction_Status_Array is
     array (Faction_Id) of Faction_Status_Record;

   type Root_Agrippa_Session is
     new Gnoga.Types.Connection_Data_Type with
      record
         Id           : WL.Guids.Guid;
         Main_Window  : Gnoga.Gui.Window.Pointer_To_Window_Class;
         Main_View    : Gnoga.Gui.View.View_Type;
         Dashboard    : Gnoga.Gui.Element.Common.DIV_Type;
         Info_Pane    : Gnoga.Gui.Element.Common.DIV_Type;
         Phase_Name   : Gnoga.Gui.Element.Common.Span_Type;
         Treasury     : Gnoga.Gui.Element.Common.Span_Type;
         Form         : Gnoga.Gui.Element.Form.Form_Type;
         End_Phase    : Gnoga.Gui.Element.Form.Input_Button_Type;
         Votes_Gadget : Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts
           .Voting_Chart_Gadget_Type;
         Header       : Gnoga.Gui.Element.Common.DIV_Type;
         Main         : Gnoga.Gui.Element.Common.DIV_Type;
         Footer       : Gnoga.Gui.Element.Common.DIV_Type;
         Factions     : Faction_Status_Array;
         Log          : Gnoga.Gui.Element.Common.DIV_Type;
         State        : access Agrippa.State.State_Interface'Class;
         Notifier     : access Agrippa.State.Notifications
           .Change_Handler_Interface'Class;
      end record;

   procedure Information
     (Session : in out Root_Agrippa_Session'Class;
      Class   : String;
      Message : String);

end Agrippa.Sessions;
