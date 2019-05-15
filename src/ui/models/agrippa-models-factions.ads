with Agrippa.State;
with Agrippa.Factions;

package Agrippa.Models.Factions is

   function Faction_Table_Model
     (State : not null access Agrippa.State.State_Interface'Class)
      return Model_Type;

   function Faction_Model
     (State   : not null access Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
      return Model_Type;

end Agrippa.Models.Factions;
