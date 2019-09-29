with Agrippa.Json;
with Agrippa.Game;

package Agrippa.UI.Web_UI.Handlers is

   type Handle_Get is access
     function (Game : Agrippa.Game.Game_Type)
               return Agrippa.Json.Json_Value'Class;

   procedure Register
     (Url     : String;
      Handler : Handle_Get);

   function Handle_Request
     (Url     : String;
      Game    : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

end Agrippa.UI.Web_UI.Handlers;
