with Agrippa.Models.Tables;

with Agrippa.Images;

package body Agrippa.Models.Factions is

   type Root_Faction_Model is
     new Agrippa.Models.Tables.Root_Table_Model with
      record
         Faction : Faction_Id;
      end record;

   overriding function Title
     (Model : Root_Faction_Model)
      return String;

   type Faction_Model_Access is access all Root_Faction_Model'Class;

   type Root_Faction_Table_Model is
     new Agrippa.Models.Tables.Root_Table_Model with
      record
         null;
      end record;

   type Faction_Table_Model_Access is access all Root_Faction_Table_Model;

   -------------------
   -- Faction_Model --
   -------------------

   function Faction_Model
     (State   : not null access Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
      return Model_Type
   is
      Model : constant Faction_Model_Access :=
                new Root_Faction_Model'
                  (Agrippa.Models.Tables.Root_Table_Model with
                   Faction => Faction);

      Senators : constant Senator_Id_Array :=
                   State.Faction_Senators (Faction);

   begin
      Model.Set_State (State);
      Model.Add_Column ("Name");
      Model.Add_Column ("M");
      Model.Add_Column ("I");
      Model.Add_Column ("O");
      Model.Add_Column ("L");
      Model.Add_Column ("Votes");

      for Senator of Senators loop
         declare
            Row : constant Agrippa.Models.Tables.Table_Row_Index :=
                    Model.Add_Row;
         begin
            Model.Set_Cell (Row, 1, State.Senator_Name (Senator));
            Model.Set_Cell (Row, 2, Natural (State.Military (Senator)));
            Model.Set_Cell (Row, 3, Natural (State.Influence (Senator)));
            Model.Set_Cell (Row, 4, Natural (State.Oratory (Senator)));
            Model.Set_Cell (Row, 5, Natural (State.Loyalty (Senator)));
            Model.Set_Cell (Row, 6,
                            Natural (State.Senator_Votes (Senator)));
         end;
      end loop;

      return Model_Type (Model);

   end Faction_Model;

   -------------------------
   -- Faction_Table_Model --
   -------------------------

   function Faction_Table_Model
     (State : not null access Agrippa.State.State_Interface'Class)
      return Model_Type
   is
      Model : constant Faction_Table_Model_Access :=
                new Root_Faction_Table_Model;
   begin
      Model.Set_State (State);
      Model.Add_Column ("Name");
      Model.Add_Column ("Votes");
      for Faction in Faction_Id loop
         declare
            Row_Model : constant Model_Type :=
                          Faction_Model (State, Faction);
            Row     : constant Agrippa.Models.Tables.Table_Row_Index :=
                        Model.Add_Row (Row_Model);
         begin
            Model.Set_Cell
              (Row, 1, State.Faction_Name (Faction));
            Model.Set_Cell
              (Row, 2, Natural (State.Faction_Votes (Faction)));
         end;
      end loop;
      return Model_Type (Model);
   end Faction_Table_Model;

   overriding function Title
     (Model : Root_Faction_Model)
      return String
   is
      Faction : constant Agrippa.State.Faction_State_Interface'Class :=
                  Model.State.Get_Faction_State (Model.Faction);
   begin
      return Faction.Name
        & " "
        & Model.State.Local_Text
        ("treasury",
         Agrippa.Images.Image (Faction.Treasury))
        & " "
        & Model.State.Local_Text ("influence")
        & Faction_Influence_Range'Image
        (Faction.Influence)
        & " "
        & Model.State.Local_Text ("votes")
        & " "
        & Agrippa.Images.Image (Faction.Votes);
   end Title;

end Agrippa.Models.Factions;
