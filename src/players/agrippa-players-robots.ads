package Agrippa.Players.Robots is

   type Robot_Faction_Type is
     (Conservative, Imperial, Plutocratic, Populist);

   function Robot_Player
     (Faction_Type : Robot_Faction_Type)
      return Player_Interface'Class;

end Agrippa.Players.Robots;
