package body Agrippa.UI.Web_UI.Requests is

   -------------------
   -- Faction_Names --
   -------------------

   function Faction_Names
     (Game : Agrippa.Game.Game_Type)
      return Agrippa.Json.Json_Value'Class
   is
   begin
      return Result : Agrippa.Json.Json_Array do
         for I in Faction_Id loop
            Result.Append
              (Agrippa.Json.String_Value
                 (Game.Faction_Name (I)));
         end loop;
      end return;
   end Faction_Names;

end Agrippa.UI.Web_UI.Requests;
