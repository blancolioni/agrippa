with WL.Random;

package body Agrippa.Players.Robots.Configure is

   ----------------------
   -- Configure_Robots --
   ----------------------

   procedure Configure_Robots
     (Game      : Agrippa.Game.Game_Access;
      Available : Faction_Id_Array)
   is
      Remaining : array (Faction_Id) of Boolean :=
                    (others => False);

      function Score_Imperial
        (Faction : Faction_Id) return Integer;

      function Score_Plutocrat
        (Faction : Faction_Id) return Integer;

      function Score_Conservative
        (Faction : Faction_Id) return Integer;

      function Score_Populist
        (Faction : Faction_Id) return Integer;

      procedure Set_Player
        (Robot_Faction : Robot_Faction_Type;
         Score         : not null access
           function (Faction : Faction_Id) return Integer);

      ------------------------
      -- Score_Conservative --
      ------------------------

      function Score_Conservative
        (Faction : Faction_Id) return Integer
      is
         Result : Integer := 100;
      begin
         if not Remaining (Faction) then
            return -1000;
         end if;
         for Senator of Game.Faction_Senators (Faction) loop
            Result := Result - Natural (Game.Influence (Senator));
         end loop;
         return Result;
      end Score_Conservative;

      --------------------
      -- Score_Imperial --
      --------------------

      function Score_Imperial
        (Faction : Faction_Id) return Integer
      is
         Result : Integer := 0;
      begin
         if not Remaining (Faction) then
            return -1000;
         end if;
         for Senator of Game.Faction_Senators (Faction) loop
            Result := Result + Natural (Game.Military (Senator));
         end loop;
         return Result;
      end Score_Imperial;

      ---------------------
      -- Score_Plutocrat --
      ---------------------

      function Score_Plutocrat
        (Faction : Faction_Id) return Integer
      is
         Result : Integer := 0;
      begin
         if not Remaining (Faction) then
            return -1000;
         end if;
         for Senator of Game.Faction_Senators (Faction) loop
            Result := Result + Natural (Game.Influence (Senator));
         end loop;
         return Result;
      end Score_Plutocrat;

      --------------------
      -- Score_Populist --
      --------------------

      function Score_Populist
        (Faction : Faction_Id) return Integer
      is
         Result : Integer := 0;
      begin
         if not Remaining (Faction) then
            return -1000;
         end if;
         for Senator of Game.Faction_Senators (Faction) loop
            Result := Result + Natural (Game.Oratory (Senator));
         end loop;
         return Result;
      end Score_Populist;

      ----------------
      -- Set_Player --
      ----------------

      procedure Set_Player
        (Robot_Faction : Robot_Faction_Type;
         Score         : not null access
           function (Faction : Faction_Id) return Integer)
      is
         Faction : constant Faction_Id :=
                     Game.Highest_Score (Score);
         Player  : constant Autoplayer_Interface'Class :=
                     Create_Robot_Player
                       (Agrippa.State.State_Type (Game),
                        Robot_Faction, Faction);
      begin
         Game.Set_Player (Faction, Player.Get_Player_Handler);
         Game.Set_Autoplayer (Faction, Player);
         Remaining (Faction) := False;
      end Set_Player;

   begin

      for Id of Available loop
         Remaining (Id) := True;
      end loop;

      Set_Player (Imperial, Score_Imperial'Access);
      Set_Player (Plutocratic, Score_Plutocrat'Access);
      Set_Player (Conservative, Score_Conservative'Access);
      Set_Player (Populist, Score_Populist'Access);

      for Id in Remaining'Range loop
         if Remaining (Id) then
            declare
               Index : constant Natural := WL.Random.Random_Number (0, 3);
               Robot : constant Robot_Faction_Type :=
                         Robot_Faction_Type'Val (Index);
               Player  : constant Autoplayer_Interface'Class :=
                           Create_Robot_Player
                             (Agrippa.State.State_Type (Game),
                              Robot, Id);
            begin
               Game.Set_Player (Id, Player.Get_Player_Handler);
               Game.Set_Autoplayer (Id, Player);
            end;
         end if;
      end loop;

      Game.Set_Player_Handlers;

   end Configure_Robots;

end Agrippa.Players.Robots.Configure;
