with Agrippa.Game;
with Agrippa.Json;

package Agrippa.UI.Web_UI.Requests is

   function Faction_Names
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

end Agrippa.UI.Web_UI.Requests;
