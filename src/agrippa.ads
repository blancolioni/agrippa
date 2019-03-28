package Agrippa is

   pragma Pure (Agrippa);

   Max_Legions    : constant := 25;
   Max_Fleets     : constant := 25;
   Max_Turns      : constant := 30;
   Max_Senators   : constant := 30;
   Max_Leaders    : constant := 20;
   Max_Statesmen  : constant := 30;
   Max_Factions   : constant := 6;

   subtype Die_Range is Integer range 1 .. 6;

   subtype DR_Range is Integer range 2 .. 12;

   subtype TDR_Range is Integer range 3 .. 18;

   type Turn_Number is range 0 .. Max_Turns;

   type Unrest_Level is range 0 .. 99;

   type Card_Id is private;

   type Card_Id_Array is array (Positive range <>) of Card_Id;

   No_Card : constant Card_Id;

   type Faction_Id is range 1 .. Max_Factions;

   type Show_Faction_Interface is limited interface;

   function Faction_Name
     (Show : Show_Faction_Interface;
      Faction : Faction_Id)
      return String
      is abstract;

   type Faction_Id_Array is array (Positive range <>) of Faction_Id;

   type Senator_Id is private;

   type Show_Senator_Interface is
   limited interface and Show_Faction_Interface;

   function Senator_Name
     (Show    : Show_Senator_Interface;
      Senator : Senator_Id)
      return String
   is abstract;

   function Senator_Name_And_Faction
     (Show    : Show_Senator_Interface;
      Senator : Senator_Id)
      return String
      is abstract;

   type Senator_Id_Array is array (Positive range <>) of Senator_Id;

   type Nullable_Senator_Id is private;

   function No_Senator return Nullable_Senator_Id;
   function To_Nullable_Id (Senator : Senator_Id) return Nullable_Senator_Id;
   function Has_Senator (Item : Nullable_Senator_Id) return Boolean;
   function To_Senator_Id (Item : Nullable_Senator_Id) return Senator_Id
     with Pre => Has_Senator (Item);

   type Statesman_Id is private;

   type Statesman_Id_Array is array (Positive range <>) of Statesman_Id;

   type Province_Id is private;

   type War_Id is private;

   type War_Id_Array is array (Positive range <>) of War_Id;

   type Leader_Id is private;

   type Leader_Id_Array is array (Positive range <>) of Leader_Id;

   type Concession_Id is private;

   type Concession_Id_Array is array (Positive range <>) of Concession_Id;

   type Intrigue_Id is private;

   type Intrigue_Id_Array is array (Positive range <>) of Intrigue_Id;

   type Event_Id is private;

   type Event_Id_Array is array (Positive range <>) of Event_Id;

   type Attribute_Range is range 1 .. 12;

   type Popularity_Range is range -9 .. 9;

   type Influence_Range is range 0 .. 55;

   type Faction_Influence_Range is range 0 .. 99;

   type Vote_Type is (Abstain, Nay, Aye);

   type Vote_Count is range 0 .. 200;

   type Faction_Vote_Type is array (Vote_Type) of Vote_Count;

   type Talents is range 0 .. 999;

   type Legion_Count is range 0 .. Max_Legions;
   subtype Legion_Index is Legion_Count range 1 .. Legion_Count'Last;

   type Legion_Index_Array is array (Positive range <>) of Legion_Index;
   No_Legions : constant Legion_Index_Array (1 .. 0) := (others => <>);

   type Fleet_Count is range 0 .. Max_Fleets;
   subtype Fleet_Index is Fleet_Count range 1 .. Fleet_Count'Last;

   type Fleet_Index_Array is array (Positive range <>) of Fleet_Index;
   No_Fleets : constant Fleet_Index_Array (1 .. 0) := (others => <>);

   type Office_Type is
     (Dictator, Rome_Consul, Field_Consul, Censor,
      Master_Of_Horse, Pontifex_Maximus);

   subtype Consular_Office is Office_Type range Dictator .. Field_Consul;

   function Higher_Ranking
     (Left, Right : Office_Type)
     return Boolean
   is (Left < Right);

   function Influence_Gain
     (Office : Office_Type)
      return Influence_Range;

   type Office_Array is array (Positive range <>) of Office_Type;

private

   type Card_Id is range 0 .. 200;

   No_Card : constant Card_Id := 0;

   subtype Real_Card_Id is Card_Id range 1 .. Card_Id'Last;

   type Senator_Id is range 1 .. Max_Senators;

   type Nullable_Senator_Id is range 0 .. Max_Senators;

   function No_Senator return Nullable_Senator_Id is (0);
   function To_Nullable_Id (Senator : Senator_Id) return Nullable_Senator_Id
   is (Nullable_Senator_Id (Senator));

   function Has_Senator (Item : Nullable_Senator_Id) return Boolean
   is (Item > 0);

   function To_Senator_Id (Item : Nullable_Senator_Id) return Senator_Id
   is (Senator_Id (Item));

   type Statesman_Id is range 1 .. Max_Statesmen;

   type Province_Id is range 1 .. 50;

   type War_Id is range 1 .. 50;

   type Leader_Id is range 1 .. Max_Leaders;

   type Concession_Id is range 1 .. 30;

   type Intrigue_Id is range 1 .. 30;

   type Event_Id is range 1 .. 20;

   function Influence_Gain
     (Office : Office_Type)
      return Influence_Range
   is (case Office is
          when Dictator         => 7,
          when Field_Consul     => 5,
          when Rome_Consul      => 5,
          when Censor           => 5,
          when Master_Of_Horse  => 3,
          when Pontifex_Maximus => 5);

end Agrippa;
