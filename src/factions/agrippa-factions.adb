package body Agrippa.Factions is

   --------------
   -- Add_Card --
   --------------

   procedure Add_Card
     (Faction : in out Faction_Type'Class;
      Card    : Agrippa.Cards.Card_Type'Class)
   is
   begin
      Faction.Hand.Append (Card);
   end Add_Card;

   -----------
   -- Cards --
   -----------

   function Cards
     (Faction : Faction_Type'Class)
      return Card_Id_Array
   is
   begin
      return Result : Card_Id_Array (1 .. Faction.Hand.Last_Index) do
         for I in Result'Range loop
            Result (I) := Faction.Hand (I).Id;
         end loop;
      end return;
   end Cards;

   --------------------
   -- Create_Faction --
   --------------------

   procedure Create_Faction
     (Faction : in out Faction_Type'Class;
      Id      : Faction_Id;
      Name    : String)
   is
   begin
      Faction.Active := True;
      Faction.Id := Id;
      Faction.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Create_Faction;

   -----------------
   -- Remove_Card --
   -----------------

   procedure Remove_Card
     (Faction : in out Faction_Type'Class;
      Card    : Agrippa.Cards.Card_Type'Class)
   is
      use Agrippa.Cards.Vectors;
      Position : Cursor := Faction.Hand.Find (Card);
   begin
      pragma Assert (Has_Element (Position));
      Faction.Hand.Delete (Position);
   end Remove_Card;

   ----------------
   -- Set_Leader --
   ----------------

   procedure Set_Leader
     (Faction : in out Faction_Type'Class;
      Leader  : Senator_Id)
   is
   begin
      Faction.Has_Leader := True;
      Faction.Leader := Leader;
   end Set_Leader;

   ------------------
   -- Set_Treasury --
   ------------------

   procedure Set_Treasury
     (Faction   : in out Faction_Type'Class;
      New_Value : Talents)
   is
   begin
      Faction.Treasury := New_Value;
   end Set_Treasury;

end Agrippa.Factions;
