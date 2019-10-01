with Agrippa.Json;
with Agrippa.Game;

package Agrippa.UI.Web_UI.Handlers is

   type Handle_Get is access
     function (Game : Agrippa.Game.Game_Type)
               return Agrippa.Json.Json_Value'Class;

   type Handle_Post is access
     function (Game : in out Agrippa.Game.Game_Type)
               return Agrippa.Json.Json_Value'Class;

   procedure Register
     (Url     : String;
      Handler : Handle_Get);

   procedure Register
     (Url     : String;
      Handler : Handle_Post);

   function Handle_Get_Request
     (Url     : String;
      Game    : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

   function Handle_Post_Request
     (Url     : String;
      Game    : in out Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class;

end Agrippa.UI.Web_UI.Handlers;
