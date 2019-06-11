private with Ada.Containers.Doubly_Linked_Lists;

package Agrippa.State.Senators is

   type Senator_State_Type is new Senator_State_Interface with private;

   overriding function Id
     (State : Senator_State_Type)
      return Senator_Id;

   procedure Set_Id
     (Senator : in out Senator_State_Type;
      Id      : Senator_Id);

   function In_Forum
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function In_Curia
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function In_Play
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function In_Rome
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Is_Rebel
     (Senator : Senator_State_Type'Class)
      return Boolean;

   overriding function Is_Statesman_Only
     (Senator : Senator_State_Type)
      return Boolean;

   procedure Clear_Statesman_Only
     (Senator : in out Senator_State_Type'Class)
     with Pre => Senator.Is_Statesman_Only,
     Post => not Senator.Is_Statesman_Only;

   function Prior_Consul
     (Senator : Senator_State_Type'Class)
      return Boolean;

   overriding function Victorious
     (Senator : Senator_State_Type)
      return Boolean;

   function Has_Office
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Office
     (Senator : Senator_State_Type'Class)
      return Office_Type
     with Pre => Has_Office (Senator);

   function Has_Command
     (Senator : Senator_State_Type'Class)
      return Boolean;

   function Command
     (Senator : Senator_State_Type'Class)
      return War_Id
     with Pre => Has_Command (Senator);

   overriding function Has_Faction
     (Senator : Senator_State_Type)
      return Boolean;

   overriding function Faction
     (Senator : Senator_State_Type)
      return Faction_Id;

   function Treasury
     (Senator : Senator_State_Type'Class)
      return Talents;

   function Military
     (Senator : Senator_State_Type'Class)
      return Attribute_Range;

   function Oratory
     (Senator : Senator_State_Type'Class)
      return Attribute_Range;

   function Loyalty
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
     (Senator : in out Senator_State_Type)
     with Pre => not Senator.In_Play,
     Post => Senator.In_Play
     and then not Senator.In_Forum
     and then not Senator.In_Curia
     and then not Senator.Has_Faction
     and then Senator.Treasury = 0;

   procedure Set_In_Forum
     (Senator : in out Senator_State_Type)
     with Pre => Senator.In_Play
     and then not Senator.Has_Faction
     and then not Senator.In_Forum,
     Post => Senator.In_Forum
     and then not Senator.Has_Faction
     and then Senator.In_Forum;

   procedure Set_Faction
     (Senator : in out Senator_State_Type'Class;
      Faction : Faction_Id)
     with Pre => Senator.In_Play or else Senator.Is_Statesman_Only,
     Post => not Senator.In_Forum
     and then Senator.Has_Faction
     and then Senator.Faction = Faction;

   procedure Set_Statesman
     (Senator : in out Senator_State_Type'Class;
      Faction : Faction_Id;
      Statesman : Statesman_Id)
     with Pre => not Senator.Has_Faction
     or else Senator.Faction = Faction,
     Post => (Senator.In_Play or else Senator.Is_Statesman_Only)
     and then not Senator.In_Forum
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
     and then not Senator.In_Forum
     and then Senator.Has_Faction,
     Post => Senator.In_Play
     and then Senator.Has_Office
     and then Senator.Office = Office
     and then (Office not in Consular_Office or else Senator.Prior_Consul)
     and then Senator.Influence =
       Senator'Old.Influence + Influence_Gain (Office);

   overriding function Concessions
     (Senator : Senator_State_Type)
      return Concession_Id_Array;

   overriding procedure Assign_Concession
     (Senator    : in out Senator_State_Type;
      Concession : Concession_Id);

   overriding function Has_Statesman
     (Senator : Senator_State_Type)
      return Boolean;

   overriding function Statesman
     (Senator : Senator_State_Type)
      return Statesman_Id;

   overriding procedure Assign_Statesman
     (Senator    : in out Senator_State_Type;
      Statesman  : Statesman_Id);

   procedure Attack
     (Senator : in out Senator_State_Type;
      War     : War_Id)
     with Post => not Senator.In_Rome;

   procedure Set_Victorious
     (Senator : in out Senator_State_Type'Class)
     with Pre => not Senator.In_Rome,
          Post => Senator.Victorious;

   procedure Return_To_Rome
     (Senator : in out Senator_State_Type'Class)
     with Pre => not Senator.In_Rome,
          Post => Senator.In_Rome;

   procedure Rebel
     (Senator : in out Senator_State_Type'Class)
     with Pre => not Senator.In_Rome,
     Post => Senator.Is_Rebel;

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
     and then not Senator.Has_Statesman
     and then Senator.Treasury = 0
     and then Senator.Popularity = 0
     and then Senator.Influence = 0
     and then Senator.Concessions'Length = 0;

private

   package Concession_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Concession_Id);

   type Senator_State_Type is new Senator_State_Interface with
      record
         Id                : Senator_Id;
         In_Play           : Boolean := False;
         In_Forum          : Boolean := False;
         In_Curia          : Boolean := False;
         In_Rome           : Boolean := True;
         Leading_Army      : Boolean := False;
         Is_Rebel          : Boolean := False;
         Prior_Consul      : Boolean := False;
         Has_Faction       : Boolean := False;
         Has_Office        : Boolean := False;
         Victorious        : Boolean := False;
         Has_Statesman     : Boolean := False;
         Is_Statesman_Only : Boolean := False;
         Faction           : Faction_Id;
         War               : War_Id;
         Office            : Office_Type;
         Popularity        : Popularity_Range := 0;
         Influence         : Influence_Range := 0;
         Treasury          : Talents := 0;
         Knights           : Natural := 0;
         Statesman         : Statesman_Id;
         Concessions       : Concession_Lists.List;
      end record;

   function Senator
     (State : Senator_State_Type'Class)
      return Senator_Id
   is (State.Id);

   overriding function Id
     (State : Senator_State_Type)
      return Senator_Id
   is (State.Id);

   overriding function Has_Faction
     (Senator : Senator_State_Type)
      return Boolean
   is (Senator.Has_Faction);

   function In_Forum
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.In_Forum);

   function In_Curia
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.In_Curia);

   function In_Play
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.In_Play or else Senator.Is_Statesman_Only);

   function In_Rome
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Has_Faction and then Senator.In_Rome);

   function Is_Rebel
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Is_Rebel);

   function Prior_Consul
     (Senator : Senator_State_Type'Class)
      return Boolean
   is (Senator.Prior_Consul);

   overriding function Faction
     (Senator : Senator_State_Type)
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

   overriding function Victorious
     (Senator : Senator_State_Type)
      return Boolean
   is (Senator.Victorious);

   overriding function Has_Statesman
     (Senator : Senator_State_Type)
      return Boolean
   is (Senator.Has_Statesman);

   overriding function Statesman
     (Senator : Senator_State_Type)
      return Statesman_Id
   is (Senator.Statesman);

   overriding function Is_Statesman_Only
     (Senator : Senator_State_Type)
      return Boolean
   is (Senator.Is_Statesman_Only);

end Agrippa.State.Senators;
