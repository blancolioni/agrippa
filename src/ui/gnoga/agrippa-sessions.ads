private with Ada.Containers.Indefinite_Holders;
private with WL.Guids;

with Gnoga.Types;
with Gnoga.Gui.Window;

private with Gnoga.Gui.Element.Common;
private with Gnoga.Gui.Element.Form;
private with Gnoga.Gui.View;

with Agrippa.State;
private with Agrippa.State.Notifications;
private with Agrippa.Phases.Sequence;

private with Agrippa.Messages;

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
         Holder    : Gnoga.Gui.Element.Common.DIV_Type;
         Header    : Gnoga.Gui.Element.Common.DIV_Type;
         Name      : Gnoga.Gui.Element.Common.Span_Type;
         Coins     : Gnoga.Gui.Element.Common.Span_Type;
         Treasury  : Gnoga.Gui.Element.Common.Span_Type;
         Vote_Icon : Gnoga.Gui.Element.Common.Span_Type;
         Votes     : Gnoga.Gui.Element.Common.Span_Type;
         View      : Agrippa.UI.Gnoga_UI.Views.Agrippa_View_Type;
      end record;

   type Faction_Status_Array is
     array (Faction_Id) of Faction_Status_Record;

   type Change_Handler_Access is
     access all Agrippa.State.Notifications.Change_Handler_Interface'Class;

   type Root_Agrippa_Session is
     new Gnoga.Types.Connection_Data_Type with
      record
         Id            : WL.Guids.Guid;
         Main_Window   : Gnoga.Gui.Window.Pointer_To_Window_Class;
         Main_View     : Gnoga.Gui.View.View_Type;
         Dashboard     : Gnoga.Gui.Element.Common.DIV_Type;
         Info_Pane     : Gnoga.Gui.Element.Common.DIV_Type;
         Phase_Name    : Gnoga.Gui.Element.Common.Span_Type;
         Treasury      : Gnoga.Gui.Element.Common.Span_Type;
         Unrest        : Gnoga.Gui.Element.Common.Span_Type;
         Legions       : Gnoga.Gui.Element.Common.Span_Type;
         Fleets        : Gnoga.Gui.Element.Common.Span_Type;
         Active_Wars   : Gnoga.Gui.Element.Common.Span_Type;
         Inactive_Wars : Gnoga.Gui.Element.Common.Span_Type;
         Deck_State    : Gnoga.Gui.Element.Common.Span_Type;
         HRAO          : Gnoga.Gui.Element.Common.Span_Type;
         Form          : Gnoga.Gui.Element.Form.Form_Type;
         Start_Phase   : Gnoga.Gui.Element.Form.Input_Button_Type;
         End_Phase     : Gnoga.Gui.Element.Form.Input_Button_Type;
         Votes_Gadget  : Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts
           .Voting_Chart_Gadget_Type;
         Header        : Gnoga.Gui.Element.Common.DIV_Type;
         Main          : Gnoga.Gui.Element.Common.DIV_Type;
         Footer        : Gnoga.Gui.Element.Common.DIV_Type;
         Factions      : Faction_Status_Array;
         Log           : Gnoga.Gui.Element.Common.DIV_Type;
         State         : access Agrippa.State.State_Interface'Class;
         Notifier      : Change_Handler_Access;
      end record;

   procedure Information
     (Session : in out Root_Agrippa_Session'Class;
      Class   : String;
      Message : String);

   procedure Update (Session : in out Root_Agrippa_Session'Class);

   type Gnoga_Notifier_Type is
     new Agrippa.State.Notifications.Change_Handler_Interface with
      record
         Session : Agrippa_Session;
      end record;

   overriding procedure On_Faction_Leader_Changed
     (Handler : Gnoga_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id);

   overriding procedure Send_Message
     (Handler : Gnoga_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type);

   overriding procedure Send_Notification
     (Handler : Gnoga_Notifier_Type;
      Text    : String);

   overriding procedure On_Faction_Revenue
     (Handler   : Gnoga_Notifier_Type;
      State     : Agrippa.State.State_Interface'Class;
      Faction   : Faction_Id;
      Old_Value : Talents;
      Income    : Talents;
      Provinces : Talents;
      New_Value : Talents)
   is null;

   overriding procedure On_State_Revenue
     (Handler   : Gnoga_Notifier_Type;
      Items     : Agrippa.State.State_Revenue_Array)
   is null;

end Agrippa.Sessions;
