private with Ada.Containers.Doubly_Linked_Lists;

with Agrippa.Cards.Concessions;

package Agrippa.State.Senators is

   type Senator_State_Type is tagged private;

   function Id
     (State : Senator_State_Type'Class)
      return Senator_Id;

   function In_Curia
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function In_Play
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function In_Rome
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Prior_Consul
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Has_Office
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Office
     (Senator : Senator_State_Type'Class)
      return Office_Type
     with Pre => Senator.Has_Office;

   function Has_Command
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Command
     (Senator : Senator_State_Type'Class)
      return War_Id
     with Pre => Senator.Has_Command;

   function Has_Faction
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Faction
     (Senator : Senator_State_Type'Class)
      return Faction_Id
     with Pre => Senator.Has_Faction;

   function Treasury
     (Senator : Senator_State_Type'Class)
      return Talents;

   function Oratory
     (Senator : Senator_State_Type'Class)
      return Attribute_Range;

   function Influence
     (Senator : Senator_State_Type'Class)
      return Influence_Range;

   function Votes
     (Senator : Senator_State_Type'Class)
      return Vote_Count;

   function Popularity
     (Senator : Senator_State_Type'Class)
      return Popularity_Range;

   function Knights
     (Senator : Senator_State_Type'Class)
      return Natural;

   procedure Add_Knight
     (Senator : in out Senator_State_Type'Class);

   procedure Add_Talents
     (Senator : in out Senator_State_Type'Class;
      Count   : Talents)
     with Post => Senator.Treasury = Senator.Treasury'Old + Count;

   procedure Remove_Talents
     (Senator : in out Senator_State_Type'Class;
      Count   : Talents)
     with Pre => Count <= Senator.Treasury,
     Post => Senator.Treasury = Senator.Treasury'Old - Count;

   procedure Set_Treasury
     (Senator : in out Senator_State_Type'Class;
      Count   : Talents)
     with Post => Senator.Treasury = Count;

   procedure Set_In_Play
     (Senator : in out Senator_State_Type;
      Id      : Senator_Id)
     with Pre => not Senator.In_Play,
     Post => Senator.In_Play
     and then not Senator.In_Curia
     and then not Senator.Has_Faction
     and then Senator.Treasury = 0
     and then Senator.Id = Id;

   procedure Set_In_Curia
     (Senator : in out Senator_State_Type)
     with Pre => Senator.In_Play
     and then not Senator.Has_Faction
     and then not Senator.In_Curia,
     Post => Senator.In_Curia
     and then not Senator.Has_Faction
     and then Senator.In_Curia;

   procedure Set_Faction
     (Senator : in out Senator_State_Type'Class;
      Faction : Faction_Id)
     with Pre => Senator.In_Play,
     Post => Senator.In_Play
     and then not Senator.In_Curia
     and then Senator.Has_Faction
     and then Senator.Faction = Faction;

   procedure Clear_Office
     (Senator : in out Senator_State_Type'Class)
     with Pre => Senator.In_Play
     and then Senator.Has_Office,
     Post => Senator.In_Play
     and then not Senator.Has_Office;

   procedure Set_Office
     (Senator : in out Senator_State_Type'Class;
      Office  : Office_Type)
     with Pre => Senator.In_Play
     and then not Senator.In_Curia
     and then Senator.Has_Faction,
     Post => Senator.In_Play
     and then Senator.Has_Office
     and then Senator.Office = Office
     and then (Office not in Consular_Office or else Senator.Prior_Consul)
     and then Senator.Influence =
       Senator'Old.Influence + Influence_Gain (Office);

   procedure Assign
     (Senator    : in out Senator_State_Type;
      Concession : Agrippa.Cards.Concessions.Concession_Card_Type'Class);

   procedure Attack
     (Senator : in out Senator_State_Type;
      War     : War_Id);

   procedure Add_Influence
     (Senator : in out Senator_State_Type;
      Infl    : Influence_Range);

   procedure Change_Popularity
     (Senator : in out Senator_State_Type;
      Pop     : Popularity_Range);

   procedure Kill
     (Senator : in out Senator_State_Type;
      Leader  : Boolean)
     with Pre => Senator.In_Play,
     Post => Senator.In_Play
     and then (Leader = Senator.Has_Faction);

private

   package Concession_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Agrippa.Cards.Concessions.Concession_Card_Type,
        Agrippa.Cards.Concessions."=");

   type Senator_State_Type is tagged
      record
         Id             : Senator_Id;
         In_Play        : Boolean := False;
         In_Curia       : Boolean := False;
         In_Rome        : Boolean := True;
         Leading_Army   : Boolean := False;
         Prior_Consul   : Boolean := False;
         Has_Faction    : Boolean := False;
         Has_Office     : Boolean := False;
         Faction        : Faction_Id;
         War            : War_Id;
         Office         : Office_Type;
         Popularity     : Popularity_Range := 0;
         Influence      : Influence_Range;
         Treasury       : Talents := 0;
         Knights        : Natural := 0;
         Concessions    : Concession_Lists.List;
      end record;

   function Senator
     (State : Senator_State_Type'Class)
      return Senator_Id
   is (State.Id);

   function Id
     (State : Senator_State_Type'Class)
      return Senator_Id
   is (State.Id);

   function Has_Faction
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Has_Faction);

   function In_Curia
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.In_Curia);

   function In_Play
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.In_Play);

   function In_Rome
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.In_Rome);

   function Prior_Consul
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Prior_Consul);

   function Faction
     (Senator : Senator_State_Type'Class)
      return Faction_Id
   is (Senator.Faction);

   function Has_Office
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Has_Office);

   function Office
     (Senator : Senator_State_Type'Class)
      return Office_Type
   is (Senator.Office);

   function Has_Command
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Leading_Army);

   function Command
     (Senator : Senator_State_Type'Class)
      return War_Id
   is (Senator.War);

   function Treasury
     (Senator : Senator_State_Type'Class)
      return Talents
   is (Senator.Treasury);

   function Influence
     (Senator : Senator_State_Type'Class)
      return Influence_Range
   is (Senator.Influence);

   function Popularity
     (Senator : Senator_State_Type'Class)
      return Popularity_Range
   is (Senator.Popularity);

   function Knights
     (Senator : Senator_State_Type'Class)
      return Natural
   is (Senator.Knights);

end Agrippa.State.Senators;
