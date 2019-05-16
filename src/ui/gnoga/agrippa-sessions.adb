with Ada.Unchecked_Deallocation;

with WL.Localisation;

with Gnoga.Gui.Base;

with Agrippa.Colors;
with Agrippa.Images;

with Agrippa.Game;
with Agrippa.Scenarios;

with Agrippa.Messages;

with Agrippa.Models.Factions;

with Agrippa.Players.Robots.Configure;

package body Agrippa.Sessions is

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
      Message : Agrippa.Messages.Message_Type)
   is null;

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

   type Game_Access is access all Agrippa.Game.Game_Type'Class;

   procedure New_Game
     (Game : Game_Access;
      Scenario : Agrippa.Scenarios.Scenario_Type;
      Notifier : not null access constant
        Agrippa.State.Notifications.Change_Handler_Interface'Class);

   procedure On_End_Phase_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   -------------------
   -- Close_Session --
   -------------------

   procedure Close_Session (Session : in out Agrippa_Session) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Root_Agrippa_Session'Class, Agrippa_Session);
   begin
      Free (Session);
      Session := null;
   end Close_Session;

   -----------------
   -- Information --
   -----------------

   procedure Information
     (Session : in out Root_Agrippa_Session'Class;
      Class   : String;
      Message : String)
   is
   begin
      Session.Log.Put_Line ("[" & Class & "] " & Message);
   end Information;

   --------------
   -- New_Game --
   --------------

   procedure New_Game
     (Game     : Game_Access;
      Scenario : Agrippa.Scenarios.Scenario_Type;
      Notifier : not null access constant
        Agrippa.State.Notifications.Change_Handler_Interface'Class)
   is
   begin
      Game.Add_Faction ("Gladius");
      Game.Add_Faction ("Pila");
      Game.Add_Faction ("Aquarius");
      Game.Add_Faction ("Boni");
      Game.Add_Faction ("Agnus Dei");
      Game.Add_Faction ("Agricola");
      Game.Start
        (Scenario => Scenario,
         Language => WL.Localisation.To_Language ("en"),
         Notify   => Notifier);
      Agrippa.Players.Robots.Configure.Configure_Robots
        (Game.all, (1, 2, 3, 4, 5, 6));
   end New_Game;

   -----------------
   -- New_Session --
   -----------------

   function New_Session
     (Main_Window : Gnoga.Gui.Window.Pointer_To_Window_Class)
      return Agrippa_Session
   is
      Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                   Agrippa.Scenarios.Get ("early-republic");
      Game     : constant Game_Access := new Agrippa.Game.Game_Type;
   begin
      return Session : constant Agrippa_Session := new Root_Agrippa_Session do
         Session.Id := WL.Guids.New_Guid;
         Session.Main_Window := Main_Window;
         Session.Main_View.Create
           (Session.Main_Window.all);
         Session.Info_Pane.Create (Session.Main_View);
         Session.Info_Pane.Class_Name ("left-pane");
         Session.Phase_Name.Create (Session.Info_Pane);
         Session.Phase_Name.Class_Name ("info-phase-name");
         Session.Treasury.Create (Session.Info_Pane);
         Session.Treasury.Class_Name ("info-treasury");
         Session.Form.Create (Session.Info_Pane);

         Session.Dashboard.Create
           (Session.Main_View, "", "agrippa-dashboard");
         Session.Header.Create
           (Session.Dashboard, "", "agrippa-header");
         Session.Main.Create
           (Session.Dashboard, "", "agrippa-main");
         Session.Footer.Create
           (Session.Dashboard, "", "agrippa-footer");
         Session.Log.Create
           (Session.Footer, "", "agrippa-log");

         Session.Notifier :=
           new Gnoga_Notifier_Type'
             (Session => Session);
         New_Game (Game, Scenario, Session.Notifier);
         Session.State := Game;

         Session.End_Phase.Create
           (Session.Form, Session.State.Local_Text ("end-phase"));
         Session.End_Phase.Class_Name ("info-end-phase");
         Session.End_Phase.On_Click_Handler (On_End_Phase_Click'Access);

         Session.Votes_Gadget.Create
           (Session.Info_Pane, Session.State);

         for Faction in Faction_Id loop
            declare
               Status : Faction_Status_Record renames
                          Session.Factions (Faction);
               Id     : constant String :=
                          Integer'Image (-Integer (Faction));
            begin
               Status.Holder.Create (Session.Header);
               Status.Holder.Class_Name ("faction-view");
               Status.Holder.Background_Color
                 (Agrippa.Colors.To_Html_Color_String
                    (Game.Get_Faction_State (Faction).Color));
               Status.Header.Create (Status.Holder);
               Status.Header.Class_Name ("faction-header");
               Status.Name.Create
                 (Status.Header, Game.Faction_Name (Faction),
                  "faction-name" & Id);
               Status.Name.Class_Name ("faction-name");
               Status.Coins.Create (Status.Header);
               Status.Coins.Class_Name ("coins");
               Status.Treasury.Create
                 (Status.Header,
                  Agrippa.Images.Image (Game.Faction_Treasury (Faction)),
                  "faction-treasury" & Id);
               Status.Vote_Icon.Create (Status.Header);
               Status.Vote_Icon.Class_Name ("votes");
               Status.Votes.Create
                 (Status.Header,
                  Agrippa.Images.Image (Game.Faction_Votes (Faction)),
                  "faction-votes" & Id);
               Status.View.Create
                 (Status.Holder,
                  "faction" & Integer'Image (-Integer (Faction)),
                  Agrippa.Models.Factions.Faction_Model (Game, Faction));
            end;
         end loop;

      end return;
   end New_Session;

   ------------------------
   -- On_End_Phase_Click --
   ------------------------

   procedure On_End_Phase_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Session : constant Agrippa_Session :=
                  Agrippa_Session (Object.Connection_Data);
   begin
      Session.Phase_Name.Text
        (Session.State.Current_Activity);
      Session.Treasury.Text
        (Session.State.Local_Text
           ("treasury",
            Agrippa.Images.Image (Session.State.Current_Treasury)));
   end On_End_Phase_Click;

   -------------------------------
   -- On_Faction_Leader_Changed --
   -------------------------------

   overriding procedure On_Faction_Leader_Changed
     (Handler : Gnoga_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
   is
   begin
      if State.Has_Faction_Leader (Faction) then
         Handler.Session.Information
           ("politics",
            State.Local_Text
              ("senator-now-leads-faction",
               State.Senator_Name
                 (State.Faction_Leader (Faction)),
               State.Faction_Name (Faction)));
      end if;

   end On_Faction_Leader_Changed;

   -----------------------
   -- Send_Notification --
   -----------------------

   overriding procedure Send_Notification
     (Handler : Gnoga_Notifier_Type;
      Text    : String)
   is
   begin
      Handler.Session.Information ("general", Text);
   end Send_Notification;

end Agrippa.Sessions;
