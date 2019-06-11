private with Ada.Strings.Unbounded;
private with Agrippa.Cards.Vectors;

with Agrippa.Cards;

package Agrippa.Factions is

   type Faction_Type is tagged private;

   function Active
     (Faction : Faction_Type'Class)
      return Boolean;

   function Id (Faction : Faction_Type'Class) return Faction_Id
     with Pre => Active (Faction);

   function Name (Faction : Faction_Type'Class) return String
     with Pre => Active (Faction);

   function Treasury (Faction : Faction_Type'Class) return Talents
     with Pre => Active (Faction);

   procedure Set_Treasury
     (Faction : in out Faction_Type'Class;
      New_Value : Talents);

   procedure Create_Faction
     (Faction : in out Faction_Type'Class;
      Id      : Faction_Id;
      Name    : String)
     with Pre => not Active (Faction),
     Post => Active (Faction)
     and then Faction.Id = Id
     and then Faction.Name = Name;

   function Has_Leader
     (Faction : Faction_Type'Class)
      return Boolean;

   function Leader
     (Faction : Faction_Type'Class)
      return Senator_Id
     with Pre => Has_Leader (Faction);

   procedure Set_Leader
     (Faction : in out Faction_Type'Class;
      Leader  : Senator_Id)
     with Post => Has_Leader (Faction)
     and then Agrippa.Factions.Leader (Faction) = Leader;

   function Card_Count
     (Faction : Faction_Type'Class)
      return Natural;

   function Cards
     (Faction : Faction_Type'Class)
      return Card_Id_Array;

   procedure Add_Card
     (Faction : in out Faction_Type'Class;
      Card    : Agrippa.Cards.Card_Type'Class);

   procedure Remove_Card
     (Faction : in out Faction_Type'Class;
      Card    : Agrippa.Cards.Card_Type'Class);

private

   type Faction_Type is tagged
      record
         Active     : Boolean := False;
         Has_Leader : Boolean := False;
         Id         : Faction_Id;
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Treasury   : Talents := 0;
         Leader     : Senator_Id;
         Hand       : Agrippa.Cards.Vectors.Vector;
      end record;

   function Active
     (Faction : Faction_Type'Class)
      return Boolean
   is (Faction.Active);

   function Id (Faction : Faction_Type'Class) return Faction_Id
   is (Faction.Id);

   function Name (Faction : Faction_Type'Class) return String
   is (Ada.Strings.Unbounded.To_String (Faction.Name));

   function Treasury (Faction : Faction_Type'Class) return Talents
   is (Faction.Treasury);

   function Has_Leader
     (Faction : Faction_Type'Class)
      return Boolean
   is (Faction.Has_Leader);

   function Leader
     (Faction : Faction_Type'Class)
      return Senator_Id
   is (Faction.Leader);

   function Card_Count
     (Faction : Faction_Type'Class)
      return Natural
   is (Faction.Hand.Last_Index);

end Agrippa.Factions;
