package Agrippa.Players.Robots is

   type Robot_Faction_Type is
     (Conservative, Imperial, Plutocratic, Populist);

private

   function Create_Robot_Player
     (State        : Agrippa.State.State_Type;
      Faction_Type : Robot_Faction_Type;
      Faction      : Faction_Id)
      return Autoplayer_Interface'Class;

end Agrippa.Players.Robots;
