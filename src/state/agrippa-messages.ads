private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Agrippa.Dice;
with Agrippa.Events;
with Agrippa.Proposals;

package Agrippa.Messages is

   type Player_Action_Type is
     (Check_Rebellion, Play_Card);

   type Property_Type is (Unrest_Property, Event_Property);

   type Property_Update_Type (Property : Property_Type) is private;

   subtype Unrest_Update_Type is Property_Update_Type (Unrest_Property);
   subtype Event_Update_Type is Property_Update_Type (Event_Property);

   function Old_Unrest_Level
     (Update : Unrest_Update_Type)
      return Unrest_Level;

   function Unrest_Change
     (Update : Unrest_Update_Type)
      return Integer;

   function New_Unrest_Level
     (Update : Unrest_Update_Type)
      return Unrest_Level;

   function Event
     (Update : Event_Update_Type)
      return Agrippa.Events.Event_Type;

   function Old_Event_Level
     (Update : Event_Update_Type)
      return Natural;

   function New_Event_Level
     (Update : Event_Update_Type)
      return Natural;

   function Unrest_Change
     (Old_Level : Unrest_Level;
      Change    : Integer;
      New_Level : Unrest_Level)
      return Unrest_Update_Type;

   function Event_Status_Change
     (Event     : Agrippa.Events.Event_Type;
      Old_Level : Natural;
      New_Level : Natural)
      return Event_Update_Type;

   function Event_Occurrence
     (Event     : Agrippa.Events.Event_Type)
      return Event_Update_Type;

   type Message_Content_Type is
     (Empty_Message,
      Mortality_Roll, Faction_Transfers,
      Initiative_Roll, Persuasion_Attempt, Attract_Knights,
      Population_Roll, Make_Proposal, Proposal_Vote, Attack,
      Player_Action);

   type Message_Type
     (Content : Message_Content_Type := Empty_Message)
   is private;

   function Empty_Message return Message_Type;

   function Has_Roll (Message : Message_Type) return Boolean;
   function Get_Roll (Message : Message_Type) return Positive
     with Pre => Has_Roll (Message);

   function Has_Faction (Message : Message_Type) return Boolean;
   function Get_Faction (Message : Message_Type) return Faction_Id
     with Pre => Has_Faction (Message);

   function Has_Senator (Message : Message_Type) return Boolean;
   function Get_Senator (Message : Message_Type) return Senator_Id
     with Pre => Has_Senator (Message);

   function Has_Card (Message : Message_Type) return Boolean;
   function Get_Card (Message : Message_Type) return Card_Id
     with Pre => Has_Card (Message);

   function Legions (Message : Message_Type) return Legion_Count;
   function Fleets (Message : Message_Type) return Fleet_Count;
   function War (Message : Message_Type) return War_Id;

   function Has_Table (Message : Message_Type) return Boolean;

   procedure Add_Table_Row
     (Message : in out Message_Type;
      Heading : String;
      Value   : String;
      Comment : String := "");

   procedure Add_Table_Row
     (Message : in out Message_Type;
      Heading : String;
      Value   : Integer;
      Comment : String := "");

   procedure Scan_Table_Rows
     (Message : Message_Type;
      Process : not null access
        procedure (Heading, Value, Comment : String))
     with Pre => Has_Table (Message);

   function Has_Property_Updates
     (Message : Message_Type)
      return Boolean;

   procedure Add_Property_Update
     (Message : in out Message_Type;
      Update  : Property_Update_Type);

   procedure Scan_Property_Updates
     (Message : Message_Type;
      Process : not null access
        procedure (Update : Property_Update_Type));

   function Successful (Message : Message_Type) return Boolean;

   function Get_Money (Message : Message_Type) return Talents;

   subtype Mortality_Roll_Message is Message_Type (Mortality_Roll);

   subtype Faction_Transfers_Message is Message_Type (Faction_Transfers);

   subtype Initiative_Roll_Message is Message_Type (Initiative_Roll);

   subtype Persuasion_Attempt_Message is Message_Type (Persuasion_Attempt);

   subtype Attract_Knights_Message is Message_Type (Attract_Knights);

   subtype Make_Proposal_Message is Message_Type (Make_Proposal);

   subtype Proposal_Vote_Message is Message_Type (Proposal_Vote);

   subtype Player_Action_Message is Message_Type (Player_Action);

   function Mortality_Roll_Twice
     (Message : Mortality_Roll_Message)
      return Boolean;

   function Mortality_Roll_None
     (Message : Mortality_Roll_Message)
      return Boolean;

   function Mortality_Roll return Message_Type;

   function Mortality_Senator_Dies
     (Roll    : Positive;
      Senator : Senator_Id)
      return Message_Type;

   function Mortality_None
     (Roll    : Positive)
      return Message_Type;

   function Mortality_Roll_Twice
     (Roll    : Positive)
      return Message_Type;

   function Event_Initiative_Roll
     (Message : Initiative_Roll_Message)
     return Boolean;

   function Event_Type
     (Message : Initiative_Roll_Message)
      return Agrippa.Dice.TDR_Range
     with Pre => Event_Initiative_Roll (Message);

   function Initiative_Card
     (Message : Initiative_Roll_Message)
      return Card_Id
     with Pre => not Event_Initiative_Roll (Message);

   function Faction_Transfers
     (Faction : Faction_Id)
      return Faction_Transfers_Message;

   procedure Add_Transfer_To_Senator
     (Message    : in out Faction_Transfers_Message;
      To_Senator : Senator_Id;
      Amount     : Talents);

   procedure Add_Transfer_From_Senator
     (Message      : in out Faction_Transfers_Message;
      From_Senator : Senator_Id;
      Amount       : Talents);

   procedure Scan_Transfers
     (Message : Faction_Transfers_Message;
      On_Faction_Treasury : not null access
        procedure (Take   : Boolean;
                   Amount : Talents);
      On_Senator_Treasury : not null access
        procedure (Senator : Senator_Id;
                   Take    : Boolean;
                   Amount  : Talents));

   function Initiative_Roll
     (Faction : Faction_Id)
      return Initiative_Roll_Message;

   function Initiative_Roll
     (Message : Initiative_Roll_Message;
      Roll    : Agrippa.Dice.DR_Range;
      Event   : Agrippa.Dice.TDR_Range)
      return Initiative_Roll_Message;

   function Initiative_Roll
     (Message : Initiative_Roll_Message;
      Roll    : Agrippa.Dice.DR_Range;
      Card    : Card_Id)
      return Initiative_Roll_Message;

   function Persuasion_Attempt
     (Faction : Faction_Id)
      return Persuasion_Attempt_Message;

   function Persuasion_Attempt
     (Message : Persuasion_Attempt_Message;
      Senator : Senator_Id;
      Target  : Senator_Id;
      Bribe   : Talents)
      return Persuasion_Attempt_Message;

   function Get_Persuasion_Target
     (Message : Persuasion_Attempt_Message)
      return Senator_Id;

   function Attract_Knights
     (Faction : Faction_Id)
     return Attract_Knights_Message;

   function Attract_Knights
     (Message : Attract_Knights_Message;
      Senator : Senator_Id;
      Spend   : Talents)
      return Attract_Knights_Message;

   function Attract_Knights
     (Message : Attract_Knights_Message;
      Roll    : Positive;
      Success : Boolean)
      return Attract_Knights_Message;

   function Population_Roll return Message_Type;

   function Make_Proposal
     (Senator  : Senator_Id;
      Faction  : Faction_Id;
      Category : Agrippa.Proposals.Proposal_Category_Type)
      return Make_Proposal_Message;

   function Make_Consular_Nomination
     (Senator  : Senator_Id;
      Faction  : Faction_Id)
      return Make_Proposal_Message;

   function Make_Office_Nomination
     (Senator  : Senator_Id;
      Faction  : Faction_Id;
      Office   : Office_Type)
      return Make_Proposal_Message;

   function Make_Proposal
     (Senator    : Senator_Id;
      Faction    : Faction_Id;
      Categories : Agrippa.Proposals.Proposal_Category_Array)
      return Make_Proposal_Message;

   function Has_Proposal_Category
     (Message  : Make_Proposal_Message;
      Category : Agrippa.Proposals.Proposal_Category_Type)
      return Boolean;

   function Has_Proposal_Office
     (Message  : Make_Proposal_Message;
      Office   : Office_Type)
      return Boolean;

   function Proposal_Offices
     (Message  : Make_Proposal_Message)
      return Office_Array;

   function Make_Proposal
     (Message  : Make_Proposal_Message;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
      return Make_Proposal_Message;

   function Proposals
     (Message : Make_Proposal_Message)
      return Agrippa.Proposals.Proposal_Container_Type;

   function Attack
     (War       : War_Id)
      return Message_Type;

   function Attack_With
     (Message : Message_Type;
      Commander : Senator_Id;
      Legions   : Legion_Count;
      Fleets    : Fleet_Count)
      return Message_Type;

   type Player_Action_Array is
     array (Positive range <>) of Player_Action_Type;

   function Player_Action
     (Faction : Faction_Id;
      Senator : Senator_Id;
      Allowed : Player_Action_Array)
      return Message_Type;

   function Player_Action
     (Faction : Faction_Id;
      Senator : Senator_Id;
      Allowed : Player_Action_Type)
      return Message_Type;

   function Player_Action
     (Faction : Faction_Id;
      Allowed : Player_Action_Array)
      return Message_Type;

   function Player_Action
     (Faction : Faction_Id;
      Allowed : Player_Action_Type)
      return Message_Type;

   function Allowed_Actions
     (Message : Player_Action_Message)
      return Player_Action_Array;

   function Allowed_Action
     (Message : Player_Action_Message;
      Action  : Player_Action_Type)
      return Boolean;

   function Get_Action
     (Message : Player_Action_Message)
      return Player_Action_Type;

   function Play_Card_Action
     (Message : Player_Action_Message;
      Card    : Card_Id)
      return Player_Action_Message
     with Pre => Allowed_Action (Message, Play_Card);

   function Check_Rebellion_Action
     (Message : Player_Action_Message;
      Rebel   : Boolean)
      return Player_Action_Message
     with Pre => Allowed_Action (Message, Check_Rebellion);

   function Rebels
     (Message : Player_Action_Message)
      return Boolean;

private

   type Property_Update_Type (Property : Property_Type) is
      record
         Old_Level : Natural := 0;
         New_Level : Natural := 0;
         Change    : Integer := 0;
         Event     : Agrippa.Events.Event_Type;
      end record;

   package Property_Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Property_Update_Type);

   function Old_Unrest_Level
     (Update : Unrest_Update_Type)
      return Unrest_Level
   is (Unrest_Level (Update.Old_Level));

   function Unrest_Change
     (Update : Unrest_Update_Type)
      return Integer
   is (Update.Change);

   function New_Unrest_Level
     (Update : Unrest_Update_Type)
      return Unrest_Level
   is (Unrest_Level (Update.New_Level));

   function Event
     (Update : Event_Update_Type)
      return Agrippa.Events.Event_Type
   is (Update.Event);

   function Old_Event_Level
     (Update : Event_Update_Type)
      return Natural
   is (Update.Old_Level);

   function New_Event_Level
     (Update : Event_Update_Type)
      return Natural
   is (Update.New_Level);

   type Single_Transfer is
      record
         From_Faction : Boolean;
         To_Faction   : Boolean;
         From_Senator : Senator_Id;
         To_Senator   : Senator_Id;
         Amount       : Talents;
      end record;

   package Transfer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Single_Transfer);

   type Table_Row is
      record
         Heading, Value, Comment : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Table_Row_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Table_Row);

   type Allowed_Proposals is
     array (Agrippa.Proposals.Proposal_Category_Type) of Boolean;

   type Allowed_Offices_Array is
     array (Office_Type) of Boolean;

   type Allowed_Actions_Array is
     array (Player_Action_Type) of Boolean;

   type Bribery_Array is array (Faction_Id) of Talents;

   type Message_Type
     (Content : Message_Content_Type := Empty_Message) is
      record
         Has_Faction      : Boolean := False;
         Has_Senator      : Boolean := False;
         Has_Card         : Boolean := False;
         Has_Table        : Boolean := False;
         Success          : Boolean := False;
         Money            : Talents := 0;
         Roll             : Positive;
         Faction          : Faction_Id;
         Senator          : Senator_Id;
         Card             : Card_Id;
         Table            : Table_Row_Lists.List;
         Property_Updates : Property_Update_Lists.List;
         case Content is
            when Empty_Message =>
               null;
            when Mortality_Roll =>
               Roll_Twice : Boolean := False;
               No_Deaths  : Boolean := False;
            when Faction_Transfers =>
               Transfer_List : Transfer_Lists.List;
            when Initiative_Roll =>
               Event_Roll : Agrippa.Dice.TDR_Range;
               Drawn_Card    : Card_Id;
            when Persuasion_Attempt =>
               Target            : Senator_Id;
               Persuasion_Bribes : Bribery_Array := (others => 0);
            when Attract_Knights =>
               null;
            when Population_Roll =>
               null;
            when Make_Proposal =>
               Allowed_Categories  : Allowed_Proposals := (others => False);
               Allowed_Offices     : Allowed_Offices_Array :=
                                       (others => False);
               Proposal            : Agrippa.Proposals.Proposal_Container_Type;
            when Proposal_Vote =>
               Aye                 : Boolean;
            when Attack =>
               War                 : War_Id;
               Legions             : Legion_Count;
               Fleets              : Fleet_Count;
            when Player_Action =>
               Allowed_Actions     : Allowed_Actions_Array :=
                                       (others => False);
               Action              : Player_Action_Type;
               Rebelling           : Boolean := False;
         end case;
      end record;

   function Successful (Message : Message_Type) return Boolean
   is (Message.Success);

   function Get_Money (Message : Message_Type) return Talents
   is (Message.Money);

   function Has_Roll (Message : Message_Type) return Boolean
   is (Message.Content'Valid);  --  = Mortality_Roll);

   function Get_Roll (Message : Message_Type) return Positive
   is (Message.Roll);

   function Has_Faction (Message : Message_Type) return Boolean
   is (Message.Has_Faction);

   function Get_Faction (Message : Message_Type) return Faction_Id
   is (Message.Faction);

   function Has_Senator (Message : Message_Type) return Boolean
   is (Message.Has_Senator);

   function Get_Senator (Message : Message_Type) return Senator_Id
   is (Message.Senator);

   function Has_Card (Message : Message_Type) return Boolean
   is (Message.Has_Card);

   function Get_Card (Message : Message_Type) return Card_Id
   is (Message.Card);

   function Has_Table (Message : Message_Type) return Boolean
   is (Message.Has_Table);

   function Has_Property_Updates
     (Message : Message_Type)
      return Boolean
   is (not Message.Property_Updates.Is_Empty);

   function Mortality_Roll_Twice
     (Message : Mortality_Roll_Message)
      return Boolean
   is (Message.Roll_Twice);

   function Mortality_Roll_None
     (Message : Mortality_Roll_Message)
      return Boolean
   is (Message.No_Deaths);

   function Event_Initiative_Roll
     (Message : Initiative_Roll_Message)
      return Boolean
   is (Message.Roll = 7);

   function Event_Type
     (Message : Initiative_Roll_Message)
      return Agrippa.Dice.TDR_Range
   is (Message.Event_Roll);

   function Initiative_Card
     (Message : Initiative_Roll_Message)
      return Card_Id
   is (Message.Drawn_Card);

   function Get_Persuasion_Target
     (Message : Persuasion_Attempt_Message)
      return Senator_Id
   is (Message.Target);

   function Proposals
     (Message : Make_Proposal_Message)
      return Agrippa.Proposals.Proposal_Container_Type
   is (Message.Proposal);

   function Has_Proposal_Category
     (Message  : Make_Proposal_Message;
      Category : Agrippa.Proposals.Proposal_Category_Type)
      return Boolean
   is (Message.Allowed_Categories (Category));

   function Has_Proposal_Office
     (Message  : Make_Proposal_Message;
      Office   : Office_Type)
      return Boolean
   is (Message.Allowed_Offices (Office));

   function Legions (Message : Message_Type) return Legion_Count
   is (Message.Legions);

   function Fleets (Message : Message_Type) return Fleet_Count
   is (Message.Fleets);

   function War (Message : Message_Type) return War_Id
   is (Message.War);

   function Allowed_Action
     (Message : Player_Action_Message;
      Action  : Player_Action_Type)
      return Boolean
   is (Message.Allowed_Actions (Action));

   function Get_Action
     (Message : Player_Action_Message)
      return Player_Action_Type
   is (Message.Action);

   function Rebels
     (Message : Player_Action_Message)
      return Boolean
   is (Message.Rebelling);

end Agrippa.Messages;
