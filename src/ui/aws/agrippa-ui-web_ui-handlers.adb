with WL.String_Maps;

package body Agrippa.UI.Web_UI.Handlers is

   package Get_Handler_Maps is
     new WL.String_Maps (Handle_Get);

   Get_Handlers : Get_Handler_Maps.Map;

   package Post_Handler_Maps is
     new WL.String_Maps (Handle_Post);

   Post_Handlers : Post_Handler_Maps.Map;

   ------------------------
   -- Handle_Get_Request --
   ------------------------

   function Handle_Get_Request
     (Url     : String;
      Game    : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class
   is
   begin
      if Get_Handlers.Contains (Url) then
         return Get_Handlers.Element (Url) (Game);
      else
         return Agrippa.Json.Null_Value;
      end if;
   end Handle_Get_Request;

   -------------------------
   -- Handle_Post_Request --
   -------------------------

   function Handle_Post_Request
     (Url     : String;
      Game    : in out Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class
   is
   begin
      if Post_Handlers.Contains (Url) then
         return Post_Handlers.Element (Url) (Game);
      else
         return Agrippa.Json.Null_Value;
      end if;
   end Handle_Post_Request;

   --------------
   -- Register --
   --------------

   procedure Register
     (Url     : String;
      Handler : Handle_Get)
   is
   begin
      Get_Handlers.Insert (Url, Handler);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Url     : String;
      Handler : Handle_Post)
   is
   begin
      Post_Handlers.Insert (Url, Handler);
   end Register;

end Agrippa.UI.Web_UI.Handlers;
