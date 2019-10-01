with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Messages;
with AWS.Server;
with AWS.Status;
with AWS.Response;
with AWS.Response.Set;

with WL.Guids;
with WL.Localisation;
with WL.String_Maps;

with Agrippa.Json;

with Agrippa.Game;
with Agrippa.Messages;
with Agrippa.Players.Robots.Configure;
with Agrippa.Scenarios;
with Agrippa.State.Notifications;
with Agrippa.UI.Text;

with Agrippa.UI.Web_UI.Handlers;
with Agrippa.UI.Web_UI.Requests;

package body Agrippa.UI.Web_UI is

   type Game_Access is access Agrippa.Game.Game_Type;

   function Create_Default_Game
     return String;

   package Game_Maps is
     new WL.String_Maps (Game_Access);

   Games : Game_Maps.Map;

   function Service
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

   type Web_Notifier_Access is
     access all Agrippa.State.Notifications.Change_Handler_Interface'Class;

   -------------------------
   -- Create_Default_Game --
   -------------------------

   function Create_Default_Game
     return String
   is
      Guid     : constant WL.Guids.Guid :=
        WL.Guids.New_Guid;
      Game     : constant Game_Access := new Agrippa.Game.Game_Type;
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
         Phase_Number : Natural := 0;
         Step_Number  : Natural;
      begin
         for Faction in Faction_Id loop
            Phase_Number := Phase_Number + 1;
            Step_Number := 0;
            loop
               Step_Number := Step_Number + 1;
               Game.Set_Current_Activity
                 (Phase_Id (Phase_Number), Step_Number);
               declare
                  use type Agrippa.Messages.Message_Content_Type;
                  Response : constant Agrippa.Messages.Message_Type :=
                    Game.Send_Message
                      (Agrippa.Messages.Player_Action
                         (Faction, Agrippa.Messages.Play_Card));
               begin
                  exit when Response.Content = Agrippa.Messages.Empty_Message;
               end;
            end loop;
         end loop;
      end;

      Game.Start_Turn;

      return WL.Guids.To_String (Guid);
   end Create_Default_Game;

   -------------
   -- Service --
   -------------

   function Service
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      use all type AWS.Status.Request_Method;
      URI        : constant String := AWS.Status.URI (Request);
      Sent_Id    : constant String := AWS.Status.Parameter (Request, "id");
      Method     : constant AWS.Status.Request_Method :=
        AWS.Status.Method (Request);
      Get        : constant Boolean := Method = AWS.Status.GET;
      Post       : constant Boolean := Method = AWS.Status.POST;
   begin
      Ada.Text_IO.Put_Line
        (Method'Image & ": " & URI);

      if not Get and then not Post then
         Ada.Text_IO.Put_Line ("bad method");
         return AWS.Response.Build ("text/plain", "bad request");
      end if;

      if URI = "/new-game" then
         declare
            Id : constant String := Create_Default_Game;
            Response : Agrippa.Json.Json_Object;
            Data     : AWS.Response.Data;
         begin
            Response.Set_Property
              ("gameId", Id);
            Data := AWS.Response.Build ("text/json", Response.Serialize);
            AWS.Response.Set.Add_Header
              (Data,
               "Access-Control-Allow-Origin",
               "http://localhost:3000");
            return Data;
         end;
      elsif Sent_Id /= "" and then Games.Contains (Sent_Id) then
         declare
            Game : constant Game_Access :=
                     Games.Element (Sent_Id);
            Resp : constant Agrippa.Json.Json_Value'Class :=
              (if Get
               then Handlers.Handle_Get_Request (URI, Game.all)
               else Handlers.Handle_Post_Request (URI, Game.all));
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

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Message (E));
         return AWS.Response.Build
           (Content_Type    => "text/plain",
            Message_Body =>
               Ada.Exceptions.Exception_Message (E),
            Status_Code     => AWS.Messages.S500);

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

      Agrippa.UI.Web_UI.Handlers.Register
        (Url     => "/game/factions",
         Handler => Agrippa.UI.Web_UI.Requests.Faction_State'Access);

      Agrippa.UI.Web_UI.Handlers.Register
        (Url     => "/game/republic",
         Handler => Agrippa.UI.Web_UI.Requests.Republic_State'Access);

      Agrippa.UI.Web_UI.Handlers.Register
        (Url     => "/game/state",
         Handler => Agrippa.UI.Web_UI.Requests.Game_State'Access);

      Agrippa.UI.Web_UI.Handlers.Register
        (Url     => "/game/continue",
         Handler => Agrippa.UI.Web_UI.Requests.Continue'Access);

      AWS.Server.Start
        (Web_Server => WS,
         Name       => "Agrippa",
         Callback   => Service'Access,
         Port       => 8080);
      AWS.Server.Wait;
   end Start;

end Agrippa.UI.Web_UI;
