private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Vectors;
private with Ada.Finalization;

private with Agrippa.Cards.Senators;

with WL.Localisation;

with Agrippa.Dice;
with Agrippa.Events;
with Agrippa.Scenarios;

with Agrippa.Factions;
with Agrippa.Players;

with Agrippa.Phases;

with Agrippa.Cards.Decks;

with Agrippa.Messages;

with Agrippa.State.Fleets;
with Agrippa.State.Leaders;
with Agrippa.State.Legions;
with Agrippa.State.Senators;
with Agrippa.State.Wars;

with Agrippa.State.Notifications;

package Agrippa.Game is

   type Game_Type is
   limited new Agrippa.State.State_Interface with private;

   overriding function End_Of_Game
     (Game : Game_Type)
      return Boolean;

   overriding function Has_Winner
     (Game : Game_Type)
      return Boolean;

   overriding function Winning_Faction
     (Game : Game_Type)
      return Faction_Id;

   overriding function Senator_Name
     (Game    : Game_Type;
      Senator : Senator_Id)
      return String;

   overriding function Has_Faction
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean;

   overriding function Senator_Faction
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Faction_Id;

   overriding function Senator_Treasury
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Talents;

   overriding function Senator_Votes
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Vote_Count;

   overriding function Has_Office
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean;

   overriding function Has_Command
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean;

   overriding function Command
     (Game    : Game_Type;
      Senator : Senator_Id)
      return War_Id;

   overriding function Has_Faction_Leader
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Boolean;

   overriding function Faction_Leader
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Senator_Id;

   overriding function Is_Faction_Leader
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean;

   overriding function Faction_Name
     (Game    : Game_Type;
      Faction : Faction_Id)
      return String;

   overriding function Faction_Treasury
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Talents;

   overriding function Military
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Attribute_Range;

   overriding function Oratory
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Attribute_Range;

   overriding function Loyalty
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Attribute_Range;

   overriding function Influence
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Influence_Range;

   overriding function Popularity
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Popularity_Range;

   overriding function Highest_Ranking_Available_Officer
     (Game : Game_Type)
      return Senator_Id
     with Post => Game.Has_Faction
       (Highest_Ranking_Available_Officer'Result);

   function Scenario
     (Game : Game_Type'Class)
      return Agrippa.Scenarios.Scenario_Type;

   function Current_Turn
     (Game : Game_Type'Class)
      return Turn_Number;

   procedure Next_Turn (Game : in out Game_Type'Class);

   overriding function Current_Treasury
     (Game : Game_Type)
      return Talents;

   procedure Set_Treasury
     (Game         : in out Game_Type'Class;
      New_Treasury : Talents);

   function Current_Unrest
     (Game : Game_Type'Class)
      return Unrest_Level;

   function Senator
     (Game : Game_Type'Class;
      Id    : Senator_Id)
      return Agrippa.State.Senators.Senator_State_Type'Class;

   function Faction
     (Game  : Game_Type'Class;
      Id    : Faction_Id)
      return Agrippa.Factions.Faction_Type'Class;

   overriding function Faction_Votes
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Vote_Count;

   overriding function Faction_Influence
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Faction_Influence_Range;

   function Highest_Score
     (Game : Game_Type'Class;
      Score : not null access
        function (Faction : Faction_Id) return Integer)
      return Faction_Id;

   overriding function Faction_Senators
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Senator_Id_Array;

   overriding function Curia_Senators
     (Game    : Game_Type)
      return Senator_Id_Array;

   overriding function Curia_Leaders
     (Game    : Game_Type)
      return Leader_Id_Array;

   function Current_Legion_Maintenance
     (Game : Game_Type'Class)
      return Talents;

   function Current_Fleet_Maintenance
     (Game : Game_Type'Class)
      return Talents;

   overriding function Current_Legion_Cost
     (Game : Game_Type)
      return Talents;

   overriding function Current_Fleet_Cost
     (Game : Game_Type)
      return Talents;

   procedure Create_Fleet
     (Game : in out Game_Type'Class);

   procedure Create_Legion
     (Game : in out Game_Type'Class);

   overriding function Total_Fleet_Count
     (Game : Game_Type)
      return Fleet_Count;

   overriding function Total_Legion_Count
     (Game : Game_Type)
      return Legion_Count;

   function Regular_Legion_Count
     (Game : Game_Type'Class)
      return Legion_Count;

   function Veteran_Legion_Count
     (Game : Game_Type'Class)
      return Legion_Count;

   overriding function Available_Regular_Legions
     (Game : Game_Type)
      return Legion_Count;

   overriding function Is_Available
     (Game   : Game_Type;
      Legion : Legion_Index)
      return Boolean;

   overriding function Available_Veteran_Legions
     (Game : Game_Type)
      return Legion_Index_Array;

   overriding function Available_Fleets
     (Game : Game_Type)
      return Fleet_Count;

   overriding procedure Deploy_Veteran_Legions
     (Game    : in out Game_Type;
      Legions : Legion_Index_Array;
      War     : War_Id);

   overriding procedure Deploy_Regular_Legions
     (Game    : in out Game_Type;
      Count   : Legion_Count;
      War     : War_Id);

   overriding procedure Deploy_Fleets
     (Game    : in out Game_Type;
      Count   : Fleet_Count;
      War     : War_Id);

   function Forum_Deck
     (Game : Game_Type'Class)
      return Agrippa.Cards.Decks.Deck_Type'Class;

   procedure Add_Faction
     (Game   : in out Game_Type'Class;
      Name   : String);

   procedure Set_Player
     (Game    : in out Game_Type'Class;
      Faction : Faction_Id;
      Player  : Agrippa.Players.Player_Access);

   procedure Set_Autoplayer
     (Game    : in out Game_Type'Class;
      Faction : Faction_Id;
      Player  : Agrippa.Players.Autoplayer_Interface'Class);

   procedure Set_Player_Handlers
     (Game    : in out Game_Type'Class);

   procedure Start_Turn
     (Game : in out Game_Type'Class);

   procedure Start
     (Game     : in out Game_Type'Class;
      Scenario : Agrippa.Scenarios.Scenario_Type;
      Language : WL.Localisation.Language_Type;
      Notify   : not null access constant
        Agrippa.State.Notifications.Change_Handler_Interface'Class);

   procedure Stop
     (Game     : in out Game_Type'Class);

private

   type Senator_State_Array is
     array (Senator_Id) of Agrippa.State.Senators.Senator_State_Type;

   type Leader_State_Array is
     array (Leader_Id) of Agrippa.State.Leaders.Leader_State_Type;

   type Legion_State_Array is
     array (Legion_Index) of Agrippa.State.Legions.Legion_State_Type;

   type Fleet_State_Array is
     array (Fleet_Index) of Agrippa.State.Fleets.Fleet_State_Type;

   type Faction_Array is
     array (Faction_Id) of Agrippa.Factions.Faction_Type;

   package War_State_Vectors is
     new Ada.Containers.Vectors
       (War_Id, Agrippa.State.Wars.War_State_Type,
        Agrippa.State.Wars."=");

   package Phase_Holders is
     new Ada.Containers.Indefinite_Holders
       (Agrippa.Phases.Phase_Interface'Class,
        Agrippa.Phases."=");

   package Phase_State_Holders is
     new Ada.Containers.Indefinite_Holders
       (Agrippa.Phases.Phase_State_Type'Class,
        Agrippa.Phases."=");

   type Event_Table is
     array (Agrippa.Dice.TDR_Range) of Agrippa.Events.Event_Type;

   type Status_Effect is
      record
         Level     : Natural := 0;
         Timed     : Boolean := False;
         Remaining : Natural := 0;
      end record;

   type Status_Table is
     array (Agrippa.Events.Event_Type) of Status_Effect;

   package Autoplayer_Holders is
     new Ada.Containers.Indefinite_Holders
       (Agrippa.Players.Autoplayer_Interface'Class,
        Agrippa.Players."=");

   type Player_Record is
      record
         Autoplayer : Autoplayer_Holders.Holder;
         Handler    : Agrippa.Players.Player_Access;
      end record;

   type Player_Array is
     array (Faction_Id) of Player_Record;

   type Game_Type is limited
     new Ada.Finalization.Limited_Controlled
     and Agrippa.State.State_Interface with
      record
         Language         : WL.Localisation.Language_Type;
         Notifier         : access constant
           Agrippa.State.Notifications.Change_Handler_Interface'Class;
         Scenario         : Agrippa.Scenarios.Scenario_Type;
         Finished         : Boolean := False;
         Has_Winner       : Boolean := False;
         Faction_Count    : Natural := 0;
         Winning_Faction  : Faction_Id;
         Current_Turn     : Turn_Number := 1;
         Current_Phase    : Phase_Holders.Holder;
         Phase_State      : Phase_State_Holders.Holder;
         Treasury         : Talents := 100;
         Unrest           : Unrest_Level := 0;
         Senate_Adjourned : Boolean := False;
         Senator_State    : Senator_State_Array;
         Leader_State     : Leader_State_Array;
         War_State        : War_State_Vectors.Vector;
         Legion_State     : Legion_State_Array;
         Fleet_State      : Fleet_State_Array;
         Faction_State    : Faction_Array;
         Player_State     : Player_Array;
         Events           : Event_Table;
         Status           : Status_Table;
         Forum_Deck       : Agrippa.Cards.Decks.Deck_Type;
      end record;

   overriding procedure Initialize (Game : in out Game_Type);

   overriding function Local_Text
     (Game  : Game_Type;
      Tag   : String;
      Arg_1 : String := "";
      Arg_2 : String := "";
      Arg_3 : String := "";
      Arg_4 : String := "")
      return String
   is (WL.Localisation.Local_Text
       (Game.Language, Tag, Arg_1, Arg_2, Arg_3, Arg_4));

   overriding function Has_Local_Text
     (Game  : Game_Type;
      Tag   : String)
      return Boolean
   is (WL.Localisation.Has_Local_Text (Game.Language, Tag));

   overriding function Senate_Adjourned
     (Game : Game_Type)
      return Boolean
   is (Game.Senate_Adjourned);

   overriding function Event_Tag
     (Game  : Game_Type;
      Event : Agrippa.Dice.TDR_Range)
      return String;

   overriding function Current_Status
     (Game  : Game_Type;
      Event : Agrippa.Events.Event_Type)
      return Natural
   is (Game.Status (Event).Level);

   overriding procedure Start_Senate_Session
     (Game    : in out Game_Type);

   overriding procedure Clear_Status
     (Game    : in out Game_Type);

   overriding procedure Change_Unrest
     (Game    : in out Game_Type;
      Change  : Integer);

   overriding function Has_Holder
     (Game    : Game_Type;
      Office  : Office_Type)
      return Boolean;

   overriding function Office_Holder
     (Game    : Game_Type;
      Office  : Office_Type)
      return Senator_Id;

   overriding procedure Set_Office
     (Game    : in out Game_Type;
      Senator : Senator_Id;
      Office  : Office_Type);

   overriding procedure Set_Faction_Leader
     (Game    : in out Game_Type;
      Faction : Faction_Id;
      Senator : Senator_Id);

   overriding procedure Send_Message
     (Game    : in out Game_Type;
      Message : Agrippa.Messages.Message_Type);

   overriding procedure Send_Text_Notification
     (Game  : Game_Type;
      Text  : String);

   overriding function Get_War_State
     (Game       : Game_Type;
      War        : War_Id)
      return Agrippa.State.War_State_Interface'Class;

   overriding function Matching_Wars
     (Game       : Game_Type;
      Test       : not null access
        function (War : War_Id) return Boolean)
      return War_Id_Array;

   overriding procedure Evaluate_Faction_Revenue
     (Game    : in out Game_Type);

   overriding procedure Evaluate_State_Revenue
     (Game    : in out Game_Type);

   function Scenario
     (Game : Game_Type'Class)
      return Agrippa.Scenarios.Scenario_Type
   is (Game.Scenario);

   function Current_Turn
     (Game : Game_Type'Class)
      return Turn_Number
   is (Game.Current_Turn);

   overriding function Current_Treasury
     (Game : Game_Type)
      return Talents
   is (Game.Treasury);

   function Current_Unrest
     (Game : Game_Type'Class)
      return Unrest_Level
   is (Game.Unrest);

   function Senator
     (Game : Game_Type'Class;
      Id    : Senator_Id)
      return Agrippa.State.Senators.Senator_State_Type'Class
   is (Game.Senator_State (Id));

   function Faction
     (Game  : Game_Type'Class;
      Id    : Faction_Id)
      return Agrippa.Factions.Faction_Type'Class
   is (Game.Faction_State (Id));

   overriding function Has_Faction
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean
   is (Game.Senator_State (Senator).Has_Faction);

   overriding function Senator_Faction
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Faction_Id
   is (Game.Senator_State (Senator).Faction);

   overriding function Senator_Treasury
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Talents
   is (Game.Senator_State (Senator).Treasury);

   overriding function Senator_Votes
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Vote_Count
   is (Game.Senator_State (Senator).Votes);

   overriding function Faction_Name
     (Game    : Game_Type;
      Faction : Faction_Id)
      return String
   is (Game.Faction_State (Faction).Name);

   overriding function Faction_Treasury
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Talents
   is (Game.Faction_State (Faction).Treasury);

   overriding function Has_Faction_Leader
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Boolean
   is (Game.Faction_State (Faction).Has_Leader);

   overriding function Is_Faction_Leader
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean
   is (Game.Has_Faction (Senator)
       and then Game.Has_Faction_Leader (Game.Senator_Faction (Senator))
       and then Game.Faction_Leader
         (Game.Senator_Faction (Senator)) = Senator);

   overriding function Faction_Leader
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Senator_Id
   is (Game.Faction_State (Faction).Leader);

   overriding function Military
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Attribute_Range
   is (Agrippa.Cards.Senators.Senator (Senator).Military);

   overriding function Oratory
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Attribute_Range
   is (Agrippa.Cards.Senators.Senator (Senator).Oratory);

   overriding function Loyalty
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Attribute_Range
   is (Agrippa.Cards.Senators.Senator (Senator).Loyalty);

   overriding function Influence
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Influence_Range
   is (Game.Senator_State (Senator).Influence);

   overriding function Popularity
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Popularity_Range
   is (Game.Senator_State (Senator).Popularity);

   overriding function Has_Office
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean
   is (Game.Senator_State (Senator).Has_Office);

   overriding function Has_Command
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Boolean
   is (Game.Senator_State (Senator).Has_Command);

   overriding function Command
     (Game    : Game_Type;
      Senator : Senator_Id)
      return War_Id
   is (Game.Senator_State (Senator).Command);

   overriding function Has_Holder
     (Game    : Game_Type;
      Office  : Office_Type)
      return Boolean
   is (for some X of Game.Senator_State =>
          X.Has_Office and then X.Office = Office);

   overriding function Is_Available
     (Game   : Game_Type;
      Legion : Legion_Index)
      return Boolean
   is (Game.Legion_State (Legion).Created
       and then not Game.Legion_State (Legion).Deployed);

   function Forum_Deck
     (Game : Game_Type'Class)
      return Agrippa.Cards.Decks.Deck_Type'Class
   is (Game.Forum_Deck);

   overriding function End_Of_Game
     (Game : Game_Type)
      return Boolean
   is (Game.Finished);

   overriding function Has_Winner
     (Game : Game_Type)
      return Boolean
   is (Game.Has_Winner);

   overriding function Winning_Faction
     (Game : Game_Type)
      return Faction_Id
   is (Game.Winning_Faction);

end Agrippa.Game;
