private with Ada.Strings.Unbounded;

with WL.Localisation;

with Agrippa.Events;

with Agrippa.Messages;

with Agrippa.Cards.Statesmen;

package Agrippa.State is

   type State_Revenue_Item is private;

   function Tag (Item : State_Revenue_Item) return String;
   function Comment (Item : State_Revenue_Item) return String;
   function Value (Item : State_Revenue_Item) return Integer;

   function Create_State_Revenue_Item
     (Tag     : String;
      Comment : String;
      Value      : Integer)
      return State_Revenue_Item;

   type State_Revenue_Array is array (Positive range <>) of State_Revenue_Item;

   type Senator_State_Interface is interface;

   function Id
     (Senator : Senator_State_Interface)
      return Senator_Id
      is abstract;

   function Has_Faction
     (Senator : Senator_State_Interface)
      return Boolean
      is abstract;

   function Faction
     (Senator : Senator_State_Interface)
      return Faction_Id
   is abstract
     with Pre'Class => Senator.Has_Faction;

   function Victorious
     (Senator : Senator_State_Interface)
      return Boolean
      is abstract;

   function Concessions
     (Senator : Senator_State_Interface)
      return Concession_Id_Array
      is abstract;

   procedure Assign_Concession
     (Senator    : in out Senator_State_Interface;
      Concession : Concession_Id)
   is abstract;

   function Has_Statesman
     (Senator : Senator_State_Interface)
      return Boolean
      is abstract;

   function Statesman
     (Senator : Senator_State_Interface)
      return Statesman_Id
   is abstract
     with Pre'Class => Senator.Has_Statesman;

   procedure Assign_Statesman
     (Senator    : in out Senator_State_Interface;
      Statesman  : Statesman_Id)
   is abstract
     with Pre'Class =>
       not Senator.Has_Statesman
       and then Agrippa.Cards.Statesmen.Statesman (Statesman).Family
     = Senator.Id,
     Post'Class => Senator.Has_Statesman;

   function Voids_DS
     (Senator : Senator_State_Interface'Class;
      War     : War_Id)
      return Boolean
     with Post => Voids_DS'Result =
       (Senator.Has_Statesman
        and then Agrippa.Cards.Statesmen.Statesman (Senator.Statesman)
        .Voids_DS (War));

   type War_State_Interface is interface;

   function Id
     (State : War_State_Interface)
      return War_Id
      is abstract;

   function In_Play
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Imminent
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Active
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Inactive
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Drawn
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Discarded
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Unprosecuted
     (State : War_State_Interface)
      return Boolean
      is abstract;

   function Land_Strength
     (State : War_State_Interface)
      return Legion_Count
      is abstract;

   function Fleet_Support
     (State : War_State_Interface)
      return Fleet_Count
      is abstract;

   function Fleet_Strength
     (State : War_State_Interface)
      return Fleet_Count
      is abstract;

   type End_State_Interface is limited interface;

   function End_Of_Game
     (State : End_State_Interface)
      return Boolean
      is abstract;

   function Has_Winner
     (State : End_State_Interface)
      return Boolean
      is abstract
     with Pre'Class => State.End_Of_Game;

   function Winning_Faction
     (State : End_State_Interface)
      return Faction_Id
      is abstract
     with Pre'Class => State.End_Of_Game and then State.Has_Winner;

   type State_Interface is limited interface
     and End_State_Interface
     and Show_Senator_Interface
     and WL.Localisation.Localisation_Interface;

   function Current_Activity
     (State : State_Interface)
      return String
      is abstract;

   procedure Log
     (State   : State_Interface'Class;
      Message : String);

   function Current_Treasury
     (State   : State_Interface)
      return Talents
      is abstract;

   function Current_Unrest
     (State   : State_Interface)
      return Unrest_Level
      is abstract;

   function Current_Legion_Cost
     (State   : State_Interface)
      return Talents
      is abstract;

   function Current_Fleet_Cost
     (State   : State_Interface)
      return Talents
      is abstract;

   function Faction_Senators
     (State   : State_Interface;
      Faction : Faction_Id)
      return Senator_Id_Array
      is abstract;

   function Senators_In_Rome
     (State   : State_Interface)
      return Senator_Id_Array
      is abstract;

   function Faction_Cards
     (State   : State_Interface;
      Faction : Faction_Id)
      return Card_Id_Array
      is abstract;

   function Curia_Senators
     (State   : State_Interface)
      return Senator_Id_Array
      is abstract;

   function Senator_Treasury
     (State   : State_Interface;
      Senator : Senator_Id)
      return Talents
      is abstract;

   function Senator_Votes
     (State   : State_Interface;
      Senator : Senator_Id)
      return Vote_Count
      is abstract;

   function Is_Prior_Consul
     (State   : State_Interface;
      Senator : Senator_Id)
      return Boolean
      is abstract;

   function Has_Office
     (State   : State_Interface;
      Senator : Senator_Id)
      return Boolean
      is abstract;

   function Office
     (State   : State_Interface;
      Senator : Senator_Id)
      return Office_Type
      is abstract;

   function Has_Command
     (State   : State_Interface;
      Senator : Senator_Id)
      return Boolean
      is abstract;

   function Command
     (State   : State_Interface;
      Senator : Senator_Id)
      return War_Id
      is abstract;

   function Has_Faction
     (State   : State_Interface;
      Senator : Senator_Id)
      return Boolean
      is abstract;

   function Senator_Faction
     (State   : State_Interface;
      Senator : Senator_Id)
      return Faction_Id
      is abstract;

   function Faction_Treasury
     (State   : State_Interface;
      Faction : Faction_Id)
      return Talents
      is abstract;

   function Faction_Votes
     (State   : State_Interface;
      Faction : Faction_Id)
      return Vote_Count
      is abstract;

   function Faction_Influence
     (State   : State_Interface;
      Faction : Faction_Id)
      return Faction_Influence_Range
      is abstract;

   function Has_Faction_Leader
     (State   : State_Interface;
      Faction : Faction_Id)
      return Boolean
      is abstract;

   function Is_Faction_Leader
     (State   : State_Interface;
      Senator : Senator_Id)
      return Boolean
      is abstract;

   function Faction_Leader
     (State   : State_Interface;
      Faction : Faction_Id)
      return Senator_Id
      is abstract
     with Pre'Class => State.Has_Faction_Leader (Faction);

   procedure Set_Faction_Leader
     (State   : in out State_Interface;
      Faction : Faction_Id;
      Senator : Senator_Id)
   is abstract;

   function Has_Holder
     (State   : State_Interface;
      Office  : Office_Type)
      return Boolean
      is abstract;

   function Office_Holder
     (State   : State_Interface;
      Office  : Office_Type)
      return Senator_Id
      is abstract
     with Pre'Class => State.Has_Holder (Office);

   procedure Set_Office
     (State   : in out State_Interface;
      Senator : Senator_Id;
      Office  : Office_Type)
   is abstract;

   function Military
     (State : State_Interface;
      Senator : Senator_Id)
      return Attribute_Range
      is abstract;

   function Oratory
     (State   : State_Interface;
      Senator : Senator_Id)
      return Attribute_Range
      is abstract;

   function Loyalty
     (State   : State_Interface;
      Senator : Senator_Id)
      return Attribute_Range
      is abstract;

   function Influence
     (State   : State_Interface;
      Senator : Senator_Id)
      return Influence_Range
      is abstract;

   function Popularity
     (State   : State_Interface;
      Senator : Senator_Id)
      return Popularity_Range
      is abstract;

   function Get_Senator_State
     (State : State_Interface;
      Senator : Senator_Id)
      return Senator_State_Interface'Class
      is abstract;

   function Highest_Ranking_Available_Officer
     (State : State_Interface)
      return Senator_Id
      is abstract;

   function Senate_Adjourned
     (State : State_Interface)
      return Boolean
      is abstract;

   function Available_Regular_Legions
     (State : State_Interface)
      return Legion_Count
      is abstract;

   function Available_Veteran_Legions
     (State : State_Interface)
      return Legion_Index_Array
      is abstract;

   function Available_Fleets
     (State : State_Interface)
      return Fleet_Count
      is abstract;

   function Is_Available
     (State  : State_Interface;
      Legion : Legion_Index)
      return Boolean
      is abstract;

   procedure Deploy_Veteran_Legions
     (State : in out State_Interface;
      Legions : Legion_Index_Array;
      War     : War_Id)
   is abstract
     with Pre'Class =>
       (for all Legion of Legions => State.Is_Available (Legion)),
         Post'Class =>
           (for all Legion of Legions => not State.Is_Available (Legion));

   procedure Deploy_Regular_Legions
     (State   : in out State_Interface;
      Count   : Legion_Count;
      War     : War_Id)
   is abstract
     with Pre'Class => Count <= State.Available_Regular_Legions,
       Post'Class =>
         State.Available_Regular_Legions
           = State.Available_Regular_Legions'Old - Count;

   procedure Deploy_Fleets
     (State   : in out State_Interface;
      Count   : Fleet_Count;
      War     : War_Id)
   is abstract
     with Pre'Class => Count <= State.Available_Fleets,
       Post'Class =>
         State.Available_Fleets = State.Available_Fleets'Old - Count;

   function Total_Legion_Count
     (State : State_Interface)
      return Legion_Count
      is abstract;

   function Total_Fleet_Count
     (State : State_Interface)
      return Fleet_Count
      is abstract;

   function Curia_Leaders
     (State : State_Interface)
      return Leader_Id_Array
      is abstract;

   function Get_War_State
     (State : State_Interface;
      War   : War_Id)
      return War_State_Interface'Class
      is abstract;

   function Has_Commander
     (State : State_Interface;
      War  : War_Id)
      return Boolean
      is abstract;

   function Matching_Wars
     (State : State_Interface;
      Test  : not null access
        function (War : War_Id) return Boolean)
      return War_Id_Array
      is abstract;

   function Active_Wars
     (State      : State_Interface'Class)
      return War_Id_Array;

   function Inactive_Wars
     (State : State_Interface'Class)
      return War_Id_Array;

   function Unprosecuted_Wars
     (State : State_Interface'Class)
      return War_Id_Array;

   function Prosecuted_Wars
     (State : State_Interface'Class)
      return War_Id_Array;

   procedure Set_Unprosecuted
     (State : in out State_Interface;
      War   : War_Id)
   is abstract;

   function Event_Tag
     (State : State_Interface;
      Event : TDR_Range)
      return String
      is abstract;

   function Current_Status
     (State : State_Interface;
      Event : Agrippa.Events.Event_Type)
      return Natural
      is abstract;

   procedure Clear_Status
     (State : in out State_Interface)
   is abstract;

   function Send_Message
     (State   : in out State_Interface;
      Message : Agrippa.Messages.Message_Type)
      return Agrippa.Messages.Message_Type
      is abstract;

   procedure Send_Message
     (State   : in out State_Interface'Class;
      Message : Agrippa.Messages.Message_Type);

   procedure Send_Text_Notification
     (State : State_Interface;
      Text  : String)
   is abstract;

   procedure Start_Combat
     (State : in out State_Interface)
   is abstract;

   procedure Start_Senate_Session
     (State : in out State_Interface)
   is abstract;

   procedure Change_Unrest
     (State  : in out State_Interface;
      Change : Integer)
   is abstract;

   procedure Evaluate_Faction_Revenue
     (State : in out State_Interface)
   is abstract;

   procedure Evaluate_State_Revenue
     (State : in out State_Interface)
   is abstract;

private

   type State_Revenue_Item is
      record
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Comment    : Ada.Strings.Unbounded.Unbounded_String;
         Value      : Integer;
      end record;

   function Tag (Item : State_Revenue_Item) return String
   is (Ada.Strings.Unbounded.To_String (Item.Tag));

   function Comment (Item : State_Revenue_Item) return String
   is (Ada.Strings.Unbounded.To_String (Item.Comment));

   function Value (Item : State_Revenue_Item) return Integer
   is (Item.Value);

   function Create_State_Revenue_Item
     (Tag        : String;
      Comment    : String;
      Value      : Integer)
      return State_Revenue_Item
   is (State_Revenue_Item'
         (Tag     => Ada.Strings.Unbounded.To_Unbounded_String (Tag),
          Comment => Ada.Strings.Unbounded.To_Unbounded_String (Comment),
          Value   => Value));

   function Full_Name_And_Faction
     (State   : State_Interface'Class;
      Senator : Senator_Id)
      return String
   is (State.Local_Text
       ("senator-of-faction",
        State.Senator_Name (Senator),
        State.Faction_Name
          (State.Senator_Faction (Senator))));

   function Voids_DS
     (Senator : Senator_State_Interface'Class;
      War     : War_Id)
      return Boolean
   is (Senator.Has_Statesman
       and then Agrippa.Cards.Statesmen.Statesman (Senator.Statesman)
       .Voids_DS (War));

end Agrippa.State;
