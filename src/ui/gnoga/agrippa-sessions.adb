with Ada.Unchecked_Deallocation;

with WL.Localisation;

with Gnoga.Gui.Base;

with Agrippa.Colors;
with Agrippa.Images;

with Agrippa.Cards.Wars;
with Agrippa.Game;
with Agrippa.Scenarios;

with Agrippa.Models.Factions;

with Agrippa.Players.Robots.Configure;

with Agrippa.Sessions.Messages;

package body Agrippa.Sessions is

   type Game_Access is access all Agrippa.Game.Game_Type'Class;

   procedure New_Game
     (Game : Game_Access;
      Scenario : Agrippa.Scenarios.Scenario_Type;
      Notifier : not null access constant
        Agrippa.State.Notifications.Change_Handler_Interface'Class);

   procedure On_Start_Phase_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

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
      Session.Footer.Scroll_Top (1e5);
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
         Session.Unrest.Create (Session.Info_Pane);
         Session.Unrest.Class_Name ("info-unrest");
         Session.Legions.Create (Session.Info_Pane);
         Session.Legions.Class_Name ("info-legions");
         Session.Fleets.Create (Session.Info_Pane);
         Session.Fleets.Class_Name ("info-fleets");
         Session.Active_Wars.Create (Session.Info_Pane);
         Session.Active_Wars.Class_Name ("info-wars");
         Session.Inactive_Wars.Create (Session.Info_Pane);
         Session.Inactive_Wars.Class_Name ("info-wars");
         Session.Deck_State.Create (Session.Info_Pane);
         Session.Deck_State.Class_Name ("info-deck");
         Session.HRAO.Create (Session.Info_Pane);
         Session.HRAO.Class_Name ("info-hrao");

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
         Game.Start_Turn;

         Session.State := Game;

         Session.Start_Phase.Create
           (Session.Form, Session.State.Local_Text ("start-phase"));
         Session.Start_Phase.Class_Name ("info-start-phase");
         Session.Start_Phase.On_Click_Handler (On_Start_Phase_Click'Access);

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
               Status.Coins.Class_Name ("fas fa-coins");
               Status.Treasury.Create
                 (Status.Header,
                  Agrippa.Images.Image (Game.Faction_Treasury (Faction)),
                  "faction-treasury" & Id);
               Status.Vote_Icon.Create (Status.Header);
               Status.Vote_Icon.Class_Name ("far fa-thumbs-up");
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

         Session.Phase_Number :=
           Agrippa.Phases.Sequence.First_Phase;
         Session.Step_Number := 0;

--           Session.State.Set_Current_Activity
--             (Session.Phase_Number, Session.Step_Number);

         Session.Update;

      end return;
   end New_Session;

   ------------------------
   -- On_End_Phase_Click --
   ------------------------

   procedure On_End_Phase_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      null;
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

   --------------------------
   -- On_Start_Phase_Click --
   --------------------------

   procedure On_Start_Phase_Click
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Session     : constant Agrippa_Session :=
                      Agrippa_Session (Object.Connection_Data);
      Phase       : constant Agrippa.Phases.Phase_Interface'Class :=
                      Agrippa.Phases.Sequence.Phase (Session.Phase_Number);
      State       : Agrippa.Phases.Phase_State_Type'Class :=
                      Phase.Start (Session.State.all);
   begin
      Session.Step_Number := 1;
      Phase.Step (State, Session.State.all);
      Session.Phase_State.Replace_Element (State);
      Session.Update;
   end On_Start_Phase_Click;

   ------------------
   -- Send_Message --
   ------------------

   overriding procedure Send_Message
     (Handler : Gnoga_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
   is
   begin
      Agrippa.Sessions.Messages.Handle_Message (State, Message, Handler);
   end Send_Message;

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

   ------------
   -- Update --
   ------------

   procedure Update (Session : in out Root_Agrippa_Session'Class) is

      use Agrippa.Images;

      function Is_Active (War : War_Id) return Boolean
      is (Session.State.Get_War_State (War).Active);

      function Is_Inactive (War : War_Id) return Boolean
      is (Session.State.Get_War_State (War).Inactive);

      procedure Show_Wars
        (Element : in out Gnoga.Gui.Element.Common.Span_Type'Class;
         Label   : String;
         Test    : not null access
           function (War : War_Id) return Boolean);

      ---------------
      -- Show_Wars --
      ---------------

      procedure Show_Wars
        (Element : in out Gnoga.Gui.Element.Common.Span_Type'Class;
         Label   : String;
         Test    : not null access
           function (War : War_Id) return Boolean)
      is
         Match : constant War_Id_Array :=
                   Session.State.Matching_Wars (Test);
      begin
         Element.Inner_HTML ("");
         Element.Put_HTML ("<b>" & Label & "</b><br>");
         for I in Match'Range loop
            declare
               use Agrippa.Cards.Wars;
               War        : constant War_Card_Type'Class :=
                              Agrippa.Cards.Wars.War (Match (I));
            begin
               Element.Put_Line
                 (Session.State.Local_Text (War.Tag)
                  & " "
                  & Image (War.Land_Strength)
                  & "/"
                  & Image (War.Fleet_Support)
                  & "/"
                  & Image (War.Fleet_Strength));
            end;
         end loop;
      end Show_Wars;

   begin
      Session.Phase_Name.Text
        (Session.State.Local_Text
           (Agrippa.Phases.Sequence.Phase (Session.Phase_Number).Name));

      Session.Treasury.Text
        (Session.State.Local_Text
           ("treasury",
            Agrippa.Images.Image (Session.State.Current_Treasury)));
      Session.Unrest.Text
        (Session.State.Local_Text
           ("report-unrest-level",
            Agrippa.Images.Image (Natural (Session.State.Current_Unrest))));
      Session.Legions.Text
        (Session.State.Local_Text
           ("legion-count",
            Agrippa.Images.Image (Session.State.Total_Legion_Count),
            Agrippa.Images.Image (Session.State.Veteran_Legion_Count)));
      Session.Fleets.Text
        (Session.State.Local_Text
           ("fleet-count",
            Agrippa.Images.Image (Session.State.Total_Fleet_Count)));

      Show_Wars (Element => Session.Active_Wars,
                 Label   => Session.State.Local_Text ("active-wars"),
                 Test    => Is_Active'Access);

      Show_Wars (Element => Session.Inactive_Wars,
                 Label   => Session.State.Local_Text ("inactive-wars"),
                 Test    => Is_Inactive'Access);

      Session.Deck_State.Text
        (Session.State.Local_Text
           ("cards-in-deck")
         & ": "
         & Image (Session.State.Remaining_Cards)
         & "/"
         & Image
           (Session.State.Remaining_Cards
            + Session.State.Drawn_Cards));

      declare
         HRAO : constant Senator_Id :=
                  Session.State.Highest_Ranking_Available_Officer;
      begin
         Session.HRAO.Text
           (Session.State.Local_Text ("hrao")
            & ": "
            & Session.State.Local_Text
              ("senator-of-faction",
               Session.State.Senator_Name (HRAO),
               Session.State.Faction_Name
                 (Session.State.Senator_Faction (HRAO))));
      end;

   end Update;

end Agrippa.Sessions;
