with WL.String_Maps;

package body Agrippa.UI.Web_UI.Handlers is

   package Get_Handler_Maps is
     new WL.String_Maps (Handle_Get);

   Get_Handlers : Get_Handler_Maps.Map;

   --------------------
   -- Handle_Request --
   --------------------

   function Handle_Request
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
   end Handle_Request;

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

end Agrippa.UI.Web_UI.Handlers;
