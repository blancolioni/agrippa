with Agrippa.Game;
with Agrippa.Json;

package Agrippa.UI.Web_UI.Requests is

   function Game_State
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Current_Phase
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Faction_Names
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Faction_State
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Fleet_State
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Legion_State
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Republic_State
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Continue
     (Game : in out Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

end Agrippa.UI.Web_UI.Requests;
