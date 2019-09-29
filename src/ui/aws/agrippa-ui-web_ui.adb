with AWS.Server;
with AWS.Status;
with AWS.Response;
with AWS.Response.Set;

with WL.Guids;
with WL.Localisation;
with WL.String_Maps;

with Agrippa.Json;

with Agrippa.Game;
with Agrippa.Players.Robots.Configure;
with Agrippa.Scenarios;
with Agrippa.State.Notifications;
with Agrippa.UI.Text;

with Agrippa.UI.Web_UI.Handlers;
with Agrippa.UI.Web_UI.Requests;

package body Agrippa.UI.Web_UI is

   type Game_Access is access Agrippa.Game.Game_Type;

   package Game_Maps is
     new WL.String_Maps (Game_Access);

   Games : Game_Maps.Map;

   function Service
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

   type Web_Notifier_Access is
     access all Agrippa.State.Notifications.Change_Handler_Interface'Class;

   -------------
   -- Service --
   -------------

   function Service
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      URI        : constant String := AWS.Status.URI (Request);
      Sent_Id    : constant String := AWS.Status.Parameter (Request, "id");
   begin
      if URI = "/new-game" then
         declare
            Guid : constant WL.Guids.Guid :=
                     WL.Guids.New_Guid;
            Game : constant Game_Access := new Agrippa.Game.Game_Type;
            Notifier : constant Web_Notifier_Access :=
                         new Agrippa.State.Notifications
                           .Change_Handler_Interface'Class'
                             (Agrippa.UI.Text.Get_Text_Notifier);
         begin
            Game.Create_Default_Game
              (Scenario => Agrippa.Scenarios.Get ("early-republic"),
               Language => WL.Localisation.To_Language ("en"),
               Notify   => Notifier);
            Agrippa.Players.Robots.Configure.Configure_Robots
              (Game.all, (1, 2, 3, 4, 5, 6));
            Games.Insert (WL.Guids.To_String (Guid), Game);

            declare
               Response : Agrippa.Json.Json_Object;
               Data     : AWS.Response.Data;
            begin
               Response.Set_Property
                 ("gameId", WL.Guids.To_String (Guid));
               Data := AWS.Response.Build ("text/json", Response.Serialize);
               AWS.Response.Set.Add_Header
                 (Data,
                  "Access-Control-Allow-Origin",
                  "http://localhost:3000");
               return Data;
            end;
         end;
      elsif Sent_Id /= "" and then Games.Contains (Sent_Id) then
         declare
            Game : constant Game_Access :=
                     Games.Element (Sent_Id);
            Resp : constant Agrippa.Json.Json_Value'Class :=
                     Handlers.Handle_Request (URI, Game.all);
            Response : AWS.Response.Data :=
                         AWS.Response.Build
                           (Content_Type  => "text/json",
                            Message_Body  => Resp.Serialize);
         begin
            AWS.Response.Set.Add_Header
              (Response,
               "Access-Control-Allow-Origin",
               "http://localhost:3000");
            return Response;
         end;
      else
         return AWS.Response.Build
           (Content_Type  => "text/plain",
            Message_Body  => "bad request");
      end if;
   end Service;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI : in out Web_UI_Type) is
      pragma Unreferenced (Web_UI);
      WS : AWS.Server.HTTP;
   begin
      Agrippa.UI.Web_UI.Handlers.Register
        (Url     => "/game/faction-names",
         Handler => Agrippa.UI.Web_UI.Requests.Faction_Names'Access);

      AWS.Server.Start
        (Web_Server => WS,
         Name       => "Agrippa",
         Callback   => Service'Access,
         Port       => 8080);
      AWS.Server.Wait;
   end Start;

end Agrippa.UI.Web_UI;
