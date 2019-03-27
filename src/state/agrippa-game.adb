with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with WL.Numerics.Roman;
with WL.Random;

with Agrippa.Dice;
with Agrippa.Images;

with Agrippa.Proposals;

with Agrippa.Cards.Concessions;
with Agrippa.Cards.Intrigue;
with Agrippa.Cards.Leaders;
with Agrippa.Cards.Senators;
with Agrippa.Cards.Statesmen;
with Agrippa.Cards.Wars;

with Agrippa.Cards.Vectors;

package body Agrippa.Game is

   type Combat_Result is (Disaster, Standoff, Defeat, Stalemate, Victory);

   type Combat_Result_Record is
      record
         Result : Combat_Result;
         Losses : Natural;
      end record;

   function Adjudicate_Combat
     (War       : War_Id;
      DS_Voided : Boolean;
      TDR       : TDR_Range;
      Forces    : Positive;
      Result    : Integer)
      return Combat_Result_Record;

   function Count_Legions
     (Game    : Game_Type'Class;
      Test    : not null access
        function (Legion : Agrippa.State.Legions.Legion_State_Type'Class)
      return Boolean)
      return Legion_Count;

   function Get_Legions
     (Game    : Game_Type'Class;
      Test    : not null access
        function (Legion : Agrippa.State.Legions.Legion_State_Type'Class)
      return Boolean)
      return Legion_Index_Array;

   function Count_Fleets
     (Game    : Game_Type'Class;
      Test    : not null access
        function (Fleet : Agrippa.State.Fleets.Fleet_State_Type'Class)
      return Boolean)
      return Fleet_Count;

   function Mortality_Roll return Positive
   is (WL.Random.Random_Number (1, Max_Senators + 6));

   procedure Mortality_Roll
     (Game    : in out Game_Type'Class);

   procedure Notify
     (Game    : Game_Type'Class;
      Message : Agrippa.Messages.Message_Type);

   procedure Execute_Event
     (Game    : in out Game_Type'Class;
      Event   : Agrippa.Events.Event_Type);

   procedure Play_Card
     (Game    : in out Game_Type'Class;
      Faction : Faction_Id;
      Senator : Senator_Id;
      Card    : Agrippa.Cards.Card_Type'Class);

   procedure Play_Statesman
     (Game      : in out Game_Type'Class;
      Faction   : Faction_Id;
      Statesman : Statesman_Id);

   function Event_Tag
     (Event : Agrippa.Events.Event_Type)
      return String;

   procedure Apply_Population_Roll
     (Game    : in out Game_Type'Class;
      Roll    : Integer;
      Message : in out Agrippa.Messages.Message_Type);

   procedure  Hold_Vote
     (Game      : in out Game_Type'Class;
      Sponsor   : Senator_Id;
      Proposals : Agrippa.Proposals.Proposal_Container_Type);

   procedure Enact_Proposals
     (Game      : in out Game_Type'Class;
      Proposals : Agrippa.Proposals.Proposal_Container_Type);

   function War_Commander
     (Game : Game_Type'Class;
      War  : War_Id)
      return Senator_Id
     with Pre => Game.Has_Commander (War);

   function Deployed_Legions
     (Game : Game_Type'Class;
      War  : War_Id)
      return Legion_Count;

   function Deployed_Land_Strength
     (Game : Game_Type'Class;
      War  : War_Id)
      return Natural;

   function Deployed_Fleets
     (Game : Game_Type'Class;
      War  : War_Id)
      return Fleet_Count;

   procedure Recall_Forces
     (Game : in out Game_Type'Class;
      War  : War_Id);

   procedure Attack
     (Game : in out Game_Type'Class;
      War  : War_Id);

   -----------------
   -- Add_Faction --
   -----------------

   procedure Add_Faction
     (Game   : in out Game_Type'Class;
      Name   : String)
   is
      Faction : constant Faction_Id :=
                  Faction_Id (Game.Faction_Count + 1);
   begin
      Game.Faction_Count := Game.Faction_Count + 1;
      Game.Faction_State (Faction).Create_Faction (Faction, Name);
   end Add_Faction;

   -----------------------
   -- Adjudicate_Combat --
   -----------------------

   function Adjudicate_Combat
     (War       : War_Id;
      DS_Voided : Boolean;
      TDR       : TDR_Range;
      Forces    : Positive;
      Result    : Integer)
      return Combat_Result_Record
   is
      Card : constant Agrippa.Cards.Wars.War_Card_Type'Class :=
               Agrippa.Cards.Wars.War (War);
      Combat_Result_Table : constant array (3 .. 18) of Combat_Result_Record :=
                              (3 => (Defeat, Max_Legions),
                               4 => (Defeat, 4),
                               5 => (Defeat, 3),
                               6 => (Defeat, 2),
                               7 => (Defeat, 1),
                               8 => (Stalemate, 5),
                               9 => (Stalemate, 4),
                               10 => (Stalemate, 3),
                               11 => (Stalemate, 2),
                               12 => (Stalemate, 1),
                               13 => (Stalemate, 0),
                               14 => (Victory, 4),
                               15 => (Victory, 3),
                               16 => (Victory, 2),
                               17 => (Victory, 1),
                               18 => (Victory, 0));
   begin
      if not DS_Voided and then Card.Is_Disaster (TDR) then
         return (Disaster, (Forces + 1) / 2);
      elsif not DS_Voided and then Card.Is_Standoff (TDR) then
         return (Standoff, (Forces + 3) / 4);
      else
         return CR : Combat_Result_Record :=
           Combat_Result_Table
             (Integer'Max (3, Integer'Min (18, Result)))
         do
            CR.Losses := Natural'Min (CR.Losses, Forces);
         end return;
      end if;
   end Adjudicate_Combat;

   ---------------------------
   -- Apply_Population_Roll --
   ---------------------------

   procedure Apply_Population_Roll
     (Game    : in out Game_Type'Class;
      Roll    : Integer;
      Message : in out Agrippa.Messages.Message_Type)
   is
      type Roll_Result_Record is
         record
            Unrest        : Integer := 0;
            MS            : Boolean := False;
            NR            : Boolean := False;
            Mob           : Boolean := False;
            People_Revolt : Boolean := False;
         end record;

      Population_Table : constant array (-1 .. 18) of Roll_Result_Record :=
                           (18 => (Unrest => -3, others => <>),
                            17 => (Unrest => -2, others => <>),
                            16 => (Unrest => -1, others => <>),
                            11 .. 15 => (others => <>),
                            10       => (Unrest => 1, others => <>),
                            9        => (Unrest => 2, others => <>),
                            8        => (Unrest => 3, others => <>),
                            7        =>
                              (Unrest => 3, MS => True, others => <>),
                            6        =>
                              (Unrest => 4, others => <>),
                            5        =>
                              (Unrest => 4, MS => True, others => <>),
                            4        =>
                              (Unrest => 5, others => <>),
                            3        =>
                              (Unrest => 5, MS => True, others => <>),
                            2        =>
                              (Unrest => 5, NR => True, others => <>),
                            1        =>
                              (Unrest => 5, NR => True, Mob => True,
                               others => <>),
                            0        =>
                              (Unrest => 6, NR => True, Mob => True,
                               others => <>),

                            -1       =>
                              (People_Revolt => True, others => <>));
      Index : constant Integer :=
                (if Roll < Population_Table'First
                 then Population_Table'First
                 elsif Roll > Population_Table'Last
                 then Population_Table'Last
                 else Roll);
      Result : constant Roll_Result_Record :=
                 Population_Table (Index);

      New_Unrest : constant Unrest_Level :=
                     Unrest_Level
                       (Integer'Max
                          (Integer (Game.Unrest) + Result.Unrest,
                           0));
      Old_MS : constant Natural :=
                 Game.Status (Agrippa.Events.Manpower_Shortage).Level;
   begin

      if Result.Unrest /= 0 then
         Agrippa.Messages.Add_Property_Update
           (Message,
            Agrippa.Messages.Unrest_Change
              (Old_Level => Game.Unrest,
               Change    => Result.Unrest,
               New_Level => New_Unrest));
         Game.Unrest := New_Unrest;
      end if;

      if Result.MS then
         Game.Execute_Event (Agrippa.Events.Manpower_Shortage);
         Agrippa.Messages.Add_Property_Update
           (Message,
            Agrippa.Messages.Event_Status_Change
              (Event     => Agrippa.Events.Manpower_Shortage,
               Old_Level => Old_MS,
               New_Level =>
                 Game.Status (Agrippa.Events.Manpower_Shortage).Level));
      end if;

      if Result.NR then
         Game.Execute_Event (Agrippa.Events.No_Recruitment);
         Agrippa.Messages.Add_Property_Update
           (Message,
            Agrippa.Messages.Event_Occurrence
              (Agrippa.Events.No_Recruitment));
      end if;

      if Result.Mob then
         declare
            Saved_Unrest : constant Unrest_Level := Game.Unrest;
         begin
            Game.Unrest := 6;
            Game.Execute_Event (Agrippa.Events.Mob_Violence);
            Game.Unrest := Saved_Unrest;
         end;
         Agrippa.Messages.Add_Property_Update
           (Message,
            Agrippa.Messages.Event_Occurrence
              (Agrippa.Events.Mob_Violence));
      end if;

      if Result.People_Revolt then
         Game.Execute_Event (Agrippa.Events.People_Revolt);
         Agrippa.Messages.Add_Property_Update
           (Message,
            Agrippa.Messages.Event_Occurrence
              (Agrippa.Events.People_Revolt));
         Game.Finished := True;
         Game.Has_Winner := False;
      end if;

   end Apply_Population_Roll;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Game : in out Game_Type'Class;
      War  : War_Id)
   is
      Commander          : constant Senator_Id :=
                             Game.War_Commander (War);
      function Is_Deployed
        (State : Agrippa.State.Legions.Legion_State_Type'Class)
            return Boolean
      is (State.Deployed and then State.War = War);

      Legion_Ids         : Legion_Index_Array :=
                             Game.Get_Legions
                               (Is_Deployed'Access);
      Legion_Strength    : constant Natural :=
                             Game.Deployed_Land_Strength (War);
      Commander_Strength : constant Natural :=
                             Natural (Game.Military (Commander));
      Total_Attack       : constant Natural :=
                             Legion_Strength + Natural'Min
                               (Commander_Strength, Legion_Strength);
      War_Strength       : constant Natural :=
                             Natural
                               (Agrippa.Cards.Wars.War (War)
                                .Land_Strength);
      Roll               : constant TDR_Range :=
                             Agrippa.Dice.TDR;
      Combat_Bonus       : constant Integer :=
                             Total_Attack - War_Strength;
      Final_Roll         : constant Integer :=
                             Roll + Combat_Bonus;
      Result             : constant Combat_Result_Record :=
                             Adjudicate_Combat
                               (War, False, Roll, Legion_Strength, Final_Roll);
      Result_Tag         : constant String :=
                             (case Result.Result is
                                 when Disaster => "disaster",
                                 when Standoff => "stand-off",
                                 when Defeat    => "defeat",
                                 when Stalemate => "stalemate",
                                 when Victory   => "victory");
      Losses_Tag         : constant String :=
                             (if Result.Losses = 0
                              then "no-losses"
                              else "legions-lost");
   begin

      Game.War_State (War).Start_Combat;

      Game.Notifier.Send_Notification
        (Game.Local_Text
           ("senator-has-strength",
            Game.Senator_Name (Commander),
            Agrippa.Images.Image (Legion_Strength),
            Agrippa.Images.Image (Commander_Strength),
            Agrippa.Images.Image (Total_Attack)));
      Game.Notifier.Send_Notification
        (Game.Local_Text
           ("combat-bonus",
            Agrippa.Images.Image (Total_Attack),
            Agrippa.Images.Image (War_Strength),
            Agrippa.Images.Image (Total_Attack - War_Strength)));
      Game.Notifier.Send_Notification
        (Game.Local_Text
           ("attack-roll",
            Agrippa.Images.Image (Roll),
            Agrippa.Images.Image (Combat_Bonus),
            Agrippa.Images.Image (Final_Roll),
            Game.Local_Text
              (Result_Tag,
               Game.Local_Text
                 (Losses_Tag,
                  Agrippa.Images.Image
                    (Natural'Min
                         (Result.Losses,
                          Legion_Strength),
                     "legion")))));

      declare
         use Ada.Strings.Unbounded;
         Destroyed_Text   : Unbounded_String;
         Destroyed_Ids    : Legion_Index_Array (1 .. Result.Losses);
         Remaining_Count  : Natural := Legion_Ids'Length;
         Commander_Killed : Boolean := Result.Result = Defeat;
      begin

         if Result.Losses > 0 then
            for I in 1 .. Result.Losses loop
               declare
                  Index : constant Positive :=
                            WL.Random.Random_Number (1, Remaining_Count);
               begin
                  Destroyed_Ids (I) := Legion_Ids (Index);
                  Destroyed_Text := Destroyed_Text
                    & (if I = 1 then "" else ", ")
                    & WL.Numerics.Roman.Roman_Image
                    (Positive (Destroyed_Ids (I)));
                  Legion_Ids (Index) := Legion_Ids (Remaining_Count);
                  Remaining_Count := Remaining_Count - 1;
               end;
            end loop;
            Game.Notifier.Send_Notification
              (Game.Local_Text
                 ("legions-destroyed", To_String (Destroyed_Text)));

            if not Commander_Killed then
               for I in 1 .. Result.Losses loop
                  declare
                     Roll : constant Positive := Mortality_Roll;
                  begin
                     if Roll = Positive (Commander) then
                        Commander_Killed := True;
                        exit;
                     end if;
                  end;
               end loop;
            end if;

            for Id of Destroyed_Ids loop
               Game.Legion_State (Id).Destroy;
            end loop;
         end if;

         if Commander_Killed then
            Game.Senator_State (Commander).Kill
              (Game.Faction_Leader (Game.Senator_Faction (Commander))
               = Commander);
            Game.Notifier.Send_Notification
              (Game.Local_Text
                 ("commander-killed", Game.Senator_Name (Commander)));
            Game.Recall_Forces (War);
         else
            if Remaining_Count >= 1 then
               declare
                  Non_Veteran_Count : Natural := 0;
                  Non_Veteran       : Legion_Index_Array
                    (1 .. Remaining_Count);
               begin
                  for I in 1 .. Remaining_Count loop
                     declare
                        Id : constant Legion_Index := Legion_Ids (I);
                        State : Agrippa.State.Legions.Legion_State_Type
                        renames Game.Legion_State (Id);
                     begin
                        if not State.Veteran then
                           Non_Veteran_Count := Non_Veteran_Count + 1;
                           Non_Veteran (Non_Veteran_Count) := Id;
                        end if;
                     end;
                  end loop;

                  if Non_Veteran_Count > 0 then
                     declare
                        Index : constant Positive :=
                                  WL.Random.Random_Number
                                    (1, Non_Veteran_Count);
                        Id    : constant Legion_Index := Non_Veteran (Index);
                        State : Agrippa.State.Legions.Legion_State_Type
                        renames Game.Legion_State (Id);
                     begin
                        State.Make_Veteran;
                        State.Set_Loyalty (Commander);
                        Game.Notifier.Send_Notification
                          (Game.Local_Text
                             ("veteran-legion-created",
                              WL.Numerics.Roman.Roman_Image
                                (Positive (Legion_Ids (Index))),
                              Game.Senator_Name (Commander)));
                     end;
                  end if;
               end;
            end if;
         end if;

         if Result.Result = Victory then
            if not Commander_Killed then
               declare
                  Pop_Change : constant Popularity_Range :=
                                 Popularity_Range ((War_Strength + 1) / 2);
                  Infl_Change : constant Influence_Range :=
                                  Influence_Range ((War_Strength + 1) / 2);
               begin
                  Game.Notifier.Send_Notification
                    (Game.Local_Text
                       ("senator-gains-inf-and-pop",
                        Game.Senator_Name (Commander),
                        Agrippa.Images.Image (Infl_Change),
                        Agrippa.Images.Image (Pop_Change)));
                  Game.Senator_State (Commander).Add_Influence (Infl_Change);
                  Game.Senator_State (Commander).Change_Popularity
                    (Pop_Change);
                  Game.Senator_State (Commander).Set_Victorious;
               end;
            end if;

            if Game.Unrest = 0 then
               Game.Notifier.Send_Notification
                 ("unrest-remains-at-zero");
            else
               Game.Unrest := Game.Unrest - 1;
               Game.Notifier.Send_Notification
                 (Game.Local_Text
                    ("unrest-drops-by-to",
                     "1",
                     Agrippa.Images.Image (Natural (Game.Unrest))));
            end if;

            declare
               Spoils : constant Talents :=
                          Agrippa.Cards.Wars.Spoils
                            (Agrippa.Cards.Wars.War (War));
            begin
               Game.Notifier.Send_Notification
                 (Game.Local_Text
                    ("rome-gains-spoils",
                     Agrippa.Images.Image (Spoils)));
               Game.Treasury := Game.Treasury + Spoils;
            end;

            Game.War_State (War).Discard;

         end if;
      end;
   end Attack;

   ----------------------
   -- Available_Fleets --
   ----------------------

   overriding function Available_Fleets
     (Game : Game_Type)
      return Fleet_Count
   is
      function Is_Available
        (Fleet : Agrippa.State.Fleets.Fleet_State_Type'Class)
         return Boolean
      is (Fleet.Created and then not Fleet.Deployed);

   begin
      return Game.Count_Fleets (Is_Available'Access);
   end Available_Fleets;

   -------------------------------
   -- Available_Regular_Legions --
   -------------------------------

   overriding function Available_Regular_Legions
     (Game : Game_Type)
      return Legion_Count
   is
      function Is_Available_Regular
        (State : Agrippa.State.Legions.Legion_State_Type'Class)
         return Boolean
      is (State.Created
          and then not State.Veteran
          and then not State.Deployed);

   begin
      return Game.Count_Legions (Is_Available_Regular'Access);
   end Available_Regular_Legions;

   -------------------------------
   -- Available_Veteran_Legions --
   -------------------------------

   overriding function Available_Veteran_Legions
     (Game : Game_Type)
      return Legion_Index_Array
   is
      function Is_Available_Veteran
        (State : Agrippa.State.Legions.Legion_State_Type'Class)
         return Boolean
      is (State.Created
          and then State.Veteran
          and then not State.Deployed);

   begin
      return Game.Get_Legions (Is_Available_Veteran'Access);
   end Available_Veteran_Legions;

   -------------------
   -- Change_Unrest --
   -------------------

   overriding procedure Change_Unrest
     (Game    : in out Game_Type;
      Change  : Integer)
   is
      Current_Unrest : constant Natural := Natural (Game.Unrest);
      New_Unrest     : constant Integer := Integer (Current_Unrest + Change);
      Valid_Unrest   : constant Natural := Integer'Max (New_Unrest, 0);
      Report_Tag     : constant String :=
                         (if Change < 0 and then New_Unrest = Valid_Unrest
                          then "unrest-drops-by-x-to-y"
                          elsif Change < 0
                          then "unrest-drops-by-x-to-minimum"
                          else "unrest-increases-by-x-to-y");
   begin
      Game.Notifier.Send_Notification
        (Game.Local_Text
           (Report_Tag, Agrippa.Images.Image (abs Change),
            Agrippa.Images.Image (Valid_Unrest)));
      Game.Unrest := Unrest_Level (Valid_Unrest);
   end Change_Unrest;

   ------------------
   -- Clear_Status --
   ------------------

   overriding procedure Clear_Status
     (Game    : in out Game_Type)
   is
   begin
      for Effect of Game.Status loop
         if Effect.Timed then
            if Effect.Remaining = 0 then
               Effect.Level := 0;
            else
               Effect.Remaining := Effect.Remaining - 1;
            end if;
         end if;
      end loop;
   end Clear_Status;

   ------------------
   -- Count_Fleets --
   ------------------

   function Count_Fleets
     (Game    : Game_Type'Class;
      Test    : not null access
        function (Fleet : Agrippa.State.Fleets.Fleet_State_Type'Class)
      return Boolean)
      return Fleet_Count
   is
      Count : Fleet_Count := 0;
   begin
      for State of Game.Fleet_State loop
         if Test (State) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Fleets;

   -------------------
   -- Count_Legions --
   -------------------

   function Count_Legions
     (Game    : Game_Type'Class;
      Test    : not null access
        function (Legion : Agrippa.State.Legions.Legion_State_Type'Class)
      return Boolean)
      return Legion_Count
   is
   begin
      return Count : Legion_Count := 0 do
         for Legion of Game.Legion_State loop
            if Test (Legion) then
               Count := Count + 1;
            end if;
         end loop;
      end return;
   end Count_Legions;

   ------------------
   -- Create_Fleet --
   ------------------

   procedure Create_Fleet
     (Game : in out Game_Type'Class)
   is
   begin
      for State of Game.Fleet_State loop
         if not State.Created then
            State.Create;
            Game.Treasury :=
              Game.Treasury - Game.Current_Fleet_Cost;
            return;
         end if;
      end loop;

      raise Constraint_Error with "no more fleets";
   end Create_Fleet;

   -------------------
   -- Create_Legion --
   -------------------

   procedure Create_Legion
     (Game : in out Game_Type'Class)
   is
   begin
      for State of Game.Legion_State loop
         if not State.Created then
            State.Create;
            Game.Treasury :=
              Game.Treasury - Game.Current_Legion_Cost;
            return;
         end if;
      end loop;

      raise Constraint_Error with "no more legions";
   end Create_Legion;

   -------------------
   -- Curia_Leaders --
   -------------------

   overriding function Curia_Leaders
     (Game    : Game_Type)
      return Leader_Id_Array
   is
      Result : Leader_Id_Array (1 .. Max_Leaders);
      Count  : Natural := 0;
   begin
      for S of Game.Leader_State loop
         if S.In_Play and then S.In_Curia then
            Count := Count + 1;
            Result (Count) := S.Id;
         end if;
      end loop;
      return Result (1 .. Count);
   end Curia_Leaders;

   --------------------
   -- Curia_Senators --
   --------------------

   overriding function Curia_Senators
     (Game    : Game_Type)
      return Senator_Id_Array
   is
      Result : Senator_Id_Array (1 .. Max_Senators);
      Count  : Natural := 0;
   begin
      for S of Game.Senator_State loop
         if S.In_Play and then S.In_Forum then
            Count := Count + 1;
            Result (Count) := S.Id;
         end if;
      end loop;
      return Result (1 .. Count);
   end Curia_Senators;

   ----------------------
   -- Current_Activity --
   ----------------------

   overriding function Current_Activity
     (Game : Game_Type)
      return String
   is
   begin
      return Agrippa.Images.Image
        (Natural (Game.Current_Turn))
        & "."
        & Agrippa.Images.Image
        (Game.Phase_Number)
        & "."
        & Agrippa.Images.Image
        (Game.Step_Number);
   end Current_Activity;

   ------------------------
   -- Current_Fleet_Cost --
   ------------------------

   overriding function Current_Fleet_Cost
     (Game : Game_Type)
      return Talents
   is
   begin
      return Talents
        (10 + 10 * Game.Status (Agrippa.Events.Manpower_Shortage).Level);
   end Current_Fleet_Cost;

   -------------------------------
   -- Current_Fleet_Maintenance --
   -------------------------------

   function Current_Fleet_Maintenance
     (Game : Game_Type'Class)
      return Talents
   is
      pragma Unreferenced (Game);
   begin
      return 2;
   end Current_Fleet_Maintenance;

   -------------------------
   -- Current_Legion_Cost --
   -------------------------

   overriding function Current_Legion_Cost
     (Game : Game_Type)
      return Talents
   is
   begin
      return Talents
        (10 + 10 * Game.Status (Agrippa.Events.Manpower_Shortage).Level);
   end Current_Legion_Cost;

   --------------------------------
   -- Current_Legion_Maintenance --
   --------------------------------

   function Current_Legion_Maintenance
     (Game : Game_Type'Class)
      return Talents
   is
      pragma Unreferenced (Game);
   begin
      return 2;
   end Current_Legion_Maintenance;

   -------------------
   -- Deploy_Fleets --
   -------------------

   overriding procedure Deploy_Fleets
     (Game    : in out Game_Type;
      Count   : Fleet_Count;
      War     : War_Id)
   is
      Remaining : Fleet_Count := Count;
   begin
      for Fleet of Game.Fleet_State loop
         exit when Remaining = 0;
         if not Fleet.Deployed then
            Fleet.Deploy (War);
            Remaining := Remaining - 1;
         end if;
      end loop;
   end Deploy_Fleets;

   ----------------------------
   -- Deploy_Regular_Legions --
   ----------------------------

   overriding procedure Deploy_Regular_Legions
     (Game    : in out Game_Type;
      Count   : Legion_Count;
      War     : War_Id)
   is
      Remaining : Legion_Count := Count;
   begin
      for Legion of Game.Legion_State loop
         exit when Remaining = 0;
         if Legion.Created
           and then not Legion.Deployed
           and then not Legion.Veteran
         then
            Legion.Deploy (War);
            Remaining := Remaining - 1;
         end if;
      end loop;
   end Deploy_Regular_Legions;

   ----------------------------
   -- Deploy_Veteran_Legions --
   ----------------------------

   overriding procedure Deploy_Veteran_Legions
     (Game    : in out Game_Type;
      Legions : Legion_Index_Array;
      War     : War_Id)
   is
   begin
      for Legion of Legions loop
         Game.Legion_State (Legion).Deploy (War);
      end loop;
   end Deploy_Veteran_Legions;

   ---------------------
   -- Deployed_Fleets --
   ---------------------

   function Deployed_Fleets
     (Game : Game_Type'Class;
      War  : War_Id)
      return Fleet_Count
   is
      function Is_Deployed
        (Fleet : Agrippa.State.Fleets.Fleet_State_Type'Class)
         return Boolean
      is (Fleet.Deployed and then Fleet.War = War);

   begin
      return Count_Fleets (Game, Is_Deployed'Access);
   end Deployed_Fleets;

   ----------------------------
   -- Deployed_Land_Strength --
   ----------------------------

   function Deployed_Land_Strength
     (Game : Game_Type'Class;
      War  : War_Id)
      return Natural
   is
      function Is_Deployed
        (Legion : Agrippa.State.Legions.Legion_State_Type'Class)
         return Boolean
      is (Legion.Deployed and then Legion.War = War);

      Ids : constant Legion_Index_Array :=
              Game.Get_Legions (Is_Deployed'Access);
   begin
      return Result : Natural := 0 do
         for Id of Ids loop
            if Game.Legion_State (Id).Veteran then
               Result := Result + 2;
            else
               Result := Result + 1;
            end if;
         end loop;
      end return;
   end Deployed_Land_Strength;

   ----------------------
   -- Deployed_Legions --
   ----------------------

   function Deployed_Legions
     (Game : Game_Type'Class;
      War  : War_Id)
      return Legion_Count
   is
      function Is_Deployed
        (Legion : Agrippa.State.Legions.Legion_State_Type'Class)
         return Boolean
      is (Legion.Deployed and then Legion.War = War);

   begin
      return Count_Legions (Game, Is_Deployed'Access);
   end Deployed_Legions;

   ---------------------
   -- Enact_Proposals --
   ---------------------

   procedure Enact_Proposals
     (Game      : in out Game_Type'Class;
      Proposals : Agrippa.Proposals.Proposal_Container_Type)
   is

      Count : Natural := 0;

      procedure Enact (Proposal : Agrippa.Proposals.Proposal_Type);

      -----------
      -- Enact --
      -----------

      procedure Enact (Proposal : Agrippa.Proposals.Proposal_Type) is
         use Agrippa.Proposals;
      begin
         Count := Count + 1;
         case Proposal.Category is
            when No_Proposal =>
               Ada.Text_IO.Put_Line
                 ("warning: enacting no proposal");
            when Office_Nomination =>
               Game.Set_Office (Nominee (Proposal), Office (Proposal));

            when Governor_Nomination =>
               null;

            when Consul_For_Life =>
               null;

            when Recruitment =>
               for I in 1 .. Fleets (Proposal) loop
                  Game.Create_Fleet;
               end loop;
               Game.Notifier.Send_Notification
                 (Game.Local_Text
                    ("state-pays-x-for-y",
                     Agrippa.Images.Image
                       (Game.Current_Fleet_Cost
                        * Talents (Fleets (Proposal))),
                     Agrippa.Images.Image
                       (Natural (Fleets (Proposal)),
                        Game.Local_Text ("fleet"),
                        Game.Local_Text ("fleets"))));

               for I in 1 .. Legions (Proposal) loop
                  Game.Create_Legion;
               end loop;

               Game.Notifier.Send_Notification
                 (Game.Local_Text
                    ("state-pays-x-for-y",
                     Agrippa.Images.Image
                       (Game.Current_Legion_Cost
                        * Talents (Legions (Proposal))),
                     Agrippa.Images.Image
                       (Natural (Legions (Proposal)),
                        Game.Local_Text ("legion"),
                        Game.Local_Text ("legions"))));
               Game.Notifier.Send_Notification
                 (Game.Local_Text
                    ("treasury-now",
                     Agrippa.Images.Image (Game.Treasury)));

            when Attack =>

               Game.War_State (War (Proposal)).Attack;
               Game.Deploy_Fleets (Fleets (Proposal), War (Proposal));
               Game.Deploy_Regular_Legions
                 (Regular_Legions (Proposal), War (Proposal));
               Game.Deploy_Veteran_Legions
                 (Veteran_Legions (Proposal), War (Proposal));
               Game.Senator_State (Commander (Proposal)).Attack
                 (War (Proposal));

               declare
                  Reg : constant Legion_Count :=
                          Regular_Legions (Proposal);
                  Vet : constant Legion_Index_Array :=
                          Veteran_Legions (Proposal);
                  Flt : constant Fleet_Count :=
                          Fleets (Proposal);
               begin
                  Game.Notifier.Send_Notification
                    (Game.Local_Text
                       ("senator-leave-rome-to-fight",
                        Game.Senator_Name
                          (Commander (Proposal)),
                        Game.Local_Text
                          (Agrippa.Cards.Wars.War
                               (Agrippa.Proposals.War (Proposal))
                           .Tag),
                        Agrippa.Images.Image (Reg + Vet'Length),
                        Agrippa.Images.Image (Flt)));
               end;
         end case;
      end Enact;
   begin
      Agrippa.Proposals.Scan_Proposals
        (Proposals, Enact'Access);
   end Enact_Proposals;

   ------------------------------
   -- Evaluate_Faction_Revenue --
   ------------------------------

   overriding procedure Evaluate_Faction_Revenue
     (Game    : in out Game_Type)
   is
   begin
      for Faction of Game.Faction_State loop
         if Faction.Active then
            declare
               Total_Income : Talents := 0;

               function Concession_Income
                 (Cards : Concession_Id_Array)
                  return Talents;

               -----------------------
               -- Concession_Income --
               -----------------------

               function Concession_Income
                 (Cards : Concession_Id_Array)
                  return Talents
               is
                  use Agrippa.Cards.Concessions;
               begin
                  return Income : Talents := 0 do
                     for Id of Cards loop
                        if Concession (Id).Income_Source = Per_Turn then
                           Income := Income + Concession (Id).Income;
                        end if;
                     end loop;
                  end return;
               end Concession_Income;

            begin
               for Id of Game.Faction_Senators (Faction.Id) loop
                  declare
                     Income : constant Talents :=
                                (if Game.Is_Faction_Leader (Id)
                                 then 3 else 1)
                                + Talents (Game.Senator_State (Id).Knights)
                                + Concession_Income
                                  (Game.Senator_State (Id).Concessions);
                  begin
                     Total_Income := Total_Income + Income;
                     Game.Senator_State (Id).Add_Talents (Income);
                  end;
               end loop;

               Game.Notifier.On_Faction_Revenue
                 (State     => Game,
                  Faction   => Faction.Id,
                  Old_Value => Faction.Treasury,
                  Income    => Total_Income,
                  Provinces => 0,
                  New_Value => Faction.Treasury + Total_Income);
            end;
         end if;
      end loop;
   end Evaluate_Faction_Revenue;

   ----------------------------
   -- Evaluate_State_Revenue --
   ----------------------------

   overriding procedure Evaluate_State_Revenue
     (Game    : in out Game_Type)
   is
      Revenue : Agrippa.State.State_Revenue_Array (1 .. 40);
      Count   : Natural := 0;
      Final   : Talents := Game.Treasury;

      procedure New_Item
        (Tag     : String;
         Bonus   : Boolean;
         Value   : Talents;
         Comment : String := "");

      procedure Add
        (Tag     : String;
         Value   : Talents;
         Comment : String := "");

      procedure Remove
        (Tag     : String;
         Value   : Talents;
         Comment : String := "");

      procedure Check_Status
        (Event     : Agrippa.Events.Event_Type;
         Bonus     : Boolean;
         Low, High : Talents);

      ---------
      -- Add --
      ---------

      procedure Add
        (Tag     : String;
         Value   : Talents;
         Comment : String := "")
      is
      begin
         New_Item (Tag, True, Value, Comment);
      end Add;

      ------------------
      -- Check_Status --
      ------------------

      procedure Check_Status
        (Event     : Agrippa.Events.Event_Type;
         Bonus     : Boolean;
         Low, High : Talents)
      is
         Current : constant Natural := Game.Status (Event).Level;
      begin
         if Current > 0 then
            New_Item (Event_Tag (Event), Bonus,
                      (if Current = 1 then Low else High));
         end if;
      end Check_Status;

      --------------
      -- New_Item --
      --------------

      procedure New_Item
        (Tag     : String;
         Bonus   : Boolean;
         Value   : Talents;
         Comment : String := "")
      is
      begin
         Count := Count + 1;
         Revenue (Count) :=
           Agrippa.State.Create_State_Revenue_Item
             (Tag        => Tag,
              Comment    => Comment,
              Value      =>
                (if Bonus then Natural (Value) else -Natural (Value)));
      end New_Item;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (Tag     : String;
         Value   : Talents;
         Comment : String := "")
      is
      begin
         New_Item (Tag, False, Value, Comment);
      end Remove;

   begin
      Add ("Current", Game.Treasury);
      Add ("Annual Revenue", 100);
      Final := Final + 100;

      Check_Status (Agrippa.Events.Allied_Enthusiasm, True, 50, 75);

      declare
         Comment : constant String :=
                     Agrippa.Images.Image
                       (Natural (Game.Total_Legion_Count), "legion")
                     & ", "
                     & Agrippa.Images.Image
           (Natural (Game.Total_Fleet_Count), "fleet");
         Maintenance : constant Talents :=
                         Talents (Game.Total_Legion_Count)
                         * Game.Current_Legion_Maintenance
                         + Talents (Game.Total_Fleet_Count)
                         * Game.Current_Fleet_Maintenance;
      begin
         Remove ("Force Maintenance", Maintenance, Comment);
         Final := Final - Maintenance;
      end;

      declare
         use Ada.Strings.Unbounded;
         Comment : Unbounded_String;
         Total   : Talents := 0;
      begin
         for War of Game.War_State loop
            if War.Active then
               Comment := Comment
                 & (if Total = 0 then "" else ", ")
                 & Game.Local_Text (War.Tag);
               Total := Total + 10;
            end if;
         end loop;
         if Total > 0 then
            Remove ("Active wars", Total, To_String (Comment));
         end if;
      end;

      Add ("New treasury", Final);
      Game.Notifier.On_State_Revenue (Revenue (1 .. Count));
      Game.Set_Treasury (Final);
   end Evaluate_State_Revenue;

   ---------------
   -- Event_Tag --
   ---------------

   function Event_Tag
     (Event : Agrippa.Events.Event_Type)
      return String
   is
      function Replace_Underscores (S : String) return String;

      -------------------------
      -- Replace_Underscores --
      -------------------------

      function Replace_Underscores (S : String) return String is
      begin
         return Result : String := S do
            for Ch of Result loop
               if Ch = '_' then
                  Ch := '-';
               end if;
            end loop;
         end return;
      end Replace_Underscores;

   begin
      return Ada.Characters.Handling.To_Lower
        (Replace_Underscores
           (Event'Image));
   end Event_Tag;

   ---------------
   -- Event_Tag --
   ---------------

   overriding function Event_Tag
     (Game  : Game_Type;
      Event : TDR_Range)
      return String
   is
   begin
      return Event_Tag (Game.Events (Event));
   end Event_Tag;

   -------------------
   -- Execute_Event --
   -------------------

   procedure Execute_Event
     (Game    : in out Game_Type'Class;
      Event   : Agrippa.Events.Event_Type)
   is
      use all type Agrippa.Events.Event_Type;
      Status : Status_Effect renames Game.Status (Event);
   begin
      Status.Level := Status.Level + 1;
      Status.Timed := Agrippa.Events.Timed (Event);
      Status.Remaining := Agrippa.Events.Turns (Event);

      case Event is
         when Allied_Enthusiasm =>
            null;

         when Ally_Deserts =>
            null;

         when Barbarian_Raids =>
            null;

         when Drought =>
            Game.Change_Unrest (1);

         when Enemy_Ally_Deserts =>
            null;

         when Enemy_Leader_Dies =>
            null;

         when Epidemic =>
            null;

         when Evil_Omens =>
            null;

         when Internal_Disorder =>
            null;

         when Manpower_Shortage =>
            null;

         when Mob_Violence =>
            null;

         when Natural_Disaster =>
            null;

         when New_Alliance =>
            null;

         when No_Recruitment =>
            null;

         when People_Revolt =>
            null;

         when Pretender =>
            null;

         when Refuge =>
            null;

         when Rhodian_Alliance =>
            null;

         when Storm_At_Sea =>
            null;

         when Trial_Of_Verres =>
            null;

      end case;
   end Execute_Event;

   -----------------------
   -- Faction_Influence --
   -----------------------

   overriding function Faction_Influence
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Faction_Influence_Range
   is
      Ids : constant Senator_Id_Array :=
              Game.Faction_Senators (Faction);
   begin
      return Infl : Faction_Influence_Range := 0 do
         for Id of Ids loop
            if Game.Senator_State (Id).In_Rome then
               Infl := Infl
                 + Faction_Influence_Range
                 (Game.Senator_State (Id).Influence);
            end if;
         end loop;
      end return;
   end Faction_Influence;

   ----------------------
   -- Faction_Senators --
   ----------------------

   overriding function Faction_Senators
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Senator_Id_Array
   is
      Result : Senator_Id_Array (1 .. Max_Senators);
      Count  : Natural := 0;
   begin
      for S of Game.Senator_State loop
         if S.Has_Faction
           and then S.Faction = Faction
         then
            Count := Count + 1;
            Result (Count) := S.Id;
         end if;
      end loop;
      return Result (1 .. Count);
   end Faction_Senators;

   -------------------
   -- Faction_Votes --
   -------------------

   overriding function Faction_Votes
     (Game    : Game_Type;
      Faction : Faction_Id)
      return Vote_Count
   is
      Ids : constant Senator_Id_Array :=
              Game.Faction_Senators (Faction);
   begin
      return Votes : Vote_Count := 0 do
         for Id of Ids loop
            if Game.Senator_State (Id).In_Rome then
               Votes := Votes
                 + Game.Senator_State (Id).Votes;
            end if;
         end loop;
      end return;
   end Faction_Votes;

   -----------------
   -- Get_Legions --
   -----------------

   function Get_Legions
     (Game    : Game_Type'Class;
      Test    : not null access
        function (Legion : Agrippa.State.Legions.Legion_State_Type'Class)
      return Boolean)
      return Legion_Index_Array
   is
      Result : Legion_Index_Array (1 .. Max_Legions);
      Count  : Natural := 0;
   begin
      for I in Game.Legion_State'Range loop
         if Test (Game.Legion_State (I)) then
            Count := Count + 1;
            Result (Count) := I;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Legions;

   -----------------------
   -- Get_Senator_State --
   -----------------------

   overriding function Get_Senator_State
     (Game    : Game_Type;
      Senator : Senator_Id)
      return Agrippa.State.Senator_State_Interface'Class
   is
   begin
      return Game.Senator_State (Senator);
   end Get_Senator_State;

   -------------------
   -- Get_War_State --
   -------------------

   overriding function Get_War_State
     (Game       : Game_Type;
      War        : War_Id)
      return Agrippa.State.War_State_Interface'Class
   is
   begin
      return Game.War_State (War);
   end Get_War_State;

   -------------------
   -- Has_Commander --
   -------------------

   overriding function Has_Commander
     (Game : Game_Type;
      War  : War_Id)
      return Boolean
   is
   begin
      for Senator of Game.Senator_State loop
         if Senator.Has_Command
           and then Senator.Command = War
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Commander;

   ---------------------------------------
   -- Highest_Ranking_Available_Officer --
   ---------------------------------------

   overriding function Highest_Ranking_Available_Officer
     (Game : Game_Type)
      return Senator_Id
   is
   begin
      for Office in Office_Type loop
         if Game.Has_Holder (Office) then
            return Game.Office_Holder (Office);
         end if;
      end loop;

      declare
         Highest_Infl : Influence_Range := 0;
         Candidates   : Senator_Id_Array (1 .. Max_Senators);
         Count        : Natural := 0;
      begin
         for Id of Agrippa.Cards.Senators.All_Senators loop
            if Game.Senator_State (Id).In_Play
              and then Game.Has_Faction (Id)
            then
               declare
                  Infl : constant Influence_Range :=
                           Game.Influence (Id);
               begin
                  if Infl > Highest_Infl then
                     Count := 1;
                     Candidates (1) := Id;
                     Highest_Infl := Infl;
                  elsif Infl = Highest_Infl then
                     Count := Count + 1;
                     Candidates (Count) := Id;
                  end if;
               end;
            end if;
         end loop;

         return Candidates (WL.Random.Random_Number (1, Count));
      end;

   end Highest_Ranking_Available_Officer;

   -------------------
   -- Highest_Score --
   -------------------

   function Highest_Score
     (Game  : Game_Type'Class;
      Score : not null access
        function (Faction : Faction_Id) return Integer)
      return Faction_Id
   is
      Highest : Integer := Integer'First;
      Result  : Faction_Id := Faction_Id'First;
   begin
      for Id in Faction_Id loop
         if Game.Faction_State (Id).Active then
            declare
               This_Score : constant Integer := Score (Id);
            begin
               if This_Score > Highest then
                  Result := Id;
                  Highest := This_Score;
               end if;
            end;
         end if;
      end loop;
      return Result;
   end Highest_Score;

   ---------------
   -- Hold_Vote --
   ---------------

   procedure  Hold_Vote
     (Game      : in out Game_Type'Class;
      Sponsor   : Senator_Id;
      Proposals : Agrippa.Proposals.Proposal_Container_Type)
   is
      Result : Faction_Vote_Type := (others => 0);
   begin
      for Faction of Game.Faction_State loop
         if Faction.Active then
            declare
               Handler : constant Agrippa.Players.Player_Access :=
                           Game.Player_State (Faction.Id).Handler;
               Votes   : Faction_Vote_Type;
            begin
               Handler.Vote_Proposal (Game, Sponsor, Proposals);
               Handler.Get_Votes (Votes);
               Ada.Text_IO.Put
                 (Faction.Name & ": ");
               Ada.Text_IO.Set_Col (18);
               Ada.Text_IO.Put_Line
                 ((if Votes (Aye) > 0
                  then "+" & Agrippa.Images.Image (Votes (Aye))
                  else "")
                  & (if Votes (Nay) > 0
                    then "-" & Agrippa.Images.Image (Votes (Nay))
                    else ""));
               for V in Vote_Type loop
                  Result (V) := Result (V) + Votes (V);
               end loop;
            end;
         end if;
      end loop;
      Ada.Text_IO.Put_Line
        ("for: " & Agrippa.Images.Image (Result (Aye))
         & "; against: " & Agrippa.Images.Image (Result (Nay))
         & ": proposal "
         & (if Result (Aye) > Result (Nay) then "passes!" else "fails!"));

      if Result (Aye) > Result (Nay) then
         Game.Enact_Proposals (Proposals);
      end if;

   end Hold_Vote;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Game : in out Game_Type) is
   begin
      null;
   end Initialize;

   -------------------
   -- Matching_Wars --
   -------------------

   overriding function Matching_Wars
     (Game       : Game_Type;
      Test       : not null access
        function (War : War_Id) return Boolean)
      return War_Id_Array
   is
      Max : constant Natural := Natural (Game.War_State.Length);
      Result : War_Id_Array (1 .. Max);
      Count  : Natural := 0;
   begin
      for W of Game.War_State loop
         if Test (W.Id) then
            Count := Count + 1;
            Result (Count) := W.Id;
         end if;
      end loop;
      return Result (1 .. Count);
   end Matching_Wars;

   --------------------
   -- Mortality_Roll --
   --------------------

   procedure Mortality_Roll
     (Game    : in out Game_Type'Class)
   is
      Roll_Count : Natural := 1;
   begin
      while Roll_Count > 0 loop
         Roll_Count := Roll_Count - 1;
         declare
            Roll : constant Positive :=
                     WL.Random.Random_Number (1, Max_Senators + 6);
         begin
            if Roll <= Max_Senators then
               declare
                  Senator : constant Senator_Id := Senator_Id (Roll);
               begin
                  if Game.Senator_State (Senator).In_Play then
                     Game.Notifier.Send_Message
                       (Game,
                        Agrippa.Messages.Mortality_Senator_Dies
                          (Roll, Senator));
                     Game.Senator_State (Senator).Kill
                       (Game.Is_Faction_Leader (Senator));
                  else
                     Game.Notifier.Send_Message
                       (State   => Game,
                        Message => Agrippa.Messages.Mortality_None (Roll));
                  end if;
               end;
            elsif Roll in Max_Senators + 1 .. Max_Senators + 4 then
               Game.Notifier.Send_Message
                 (State   => Game,
                  Message => Agrippa.Messages.Mortality_None (Roll));
            elsif Roll in Max_Senators + 5 .. Max_Senators + 6 then
               Game.Notifier.Send_Message
                 (State   => Game,
                  Message => Agrippa.Messages.Mortality_Roll_Twice (Roll));
               Roll_Count := Roll_Count + 2;
            end if;
         end;
      end loop;
   end Mortality_Roll;

   ------------
   -- Notify --
   ------------

   procedure Notify
     (Game    : Game_Type'Class;
      Message : Agrippa.Messages.Message_Type)
   is
   begin
      Game.Notifier.Send_Message (Game, Message);
   end Notify;

   -------------------
   -- Office_Holder --
   -------------------

   overriding function Office_Holder
     (Game    : Game_Type;
      Office  : Office_Type)
      return Senator_Id
   is
   begin
      for State of Game.Senator_State loop
         if State.Has_Office and then State.Office = Office then
            return State.Id;
         end if;
      end loop;
      raise Constraint_Error with "office_holder: failed precondition";
   end Office_Holder;

   ---------------
   -- Play_Card --
   ---------------

   procedure Play_Card
     (Game    : in out Game_Type'Class;
      Faction : Faction_Id;
      Senator : Senator_Id;
      Card    : Agrippa.Cards.Card_Type'Class)
   is
      use all type Agrippa.Cards.Card_Class;
   begin
      case Card.Class is
         when Concession_Card =>
            Game.Senator_State (Senator).Assign_Concession
              (Agrippa.Cards.Concessions.Concession_Card_Type
                 (Card).Concession);

         when Intrigue_Card =>
            null;

         when Leader_Card =>
            Game.Leader_State
              (Agrippa.Cards.Leaders.Leader_Card_Type'Class (Card).Leader)
                .Set_In_Curia;

         when Senator_Card =>

            declare
               State : Agrippa.State.Senators.Senator_State_Type renames
                         Game.Senator_State
                           (Agrippa.Cards.Senators
                            .Senator_Card_Type'Class (Card).Senator);
            begin
               if State.Is_Statesman_Only then
                  Game.Notifier.Send_Notification
                    (Game.Senator_Name (State.Id)
                     & " joins statesman in faction "
                     & Game.Faction_Name (State.Faction));
                  State.Clear_Statesman_Only;
               else
                  State.Set_In_Forum;
               end if;
            end;

         when Statesman_Card =>
            Play_Statesman
              (Game      => Game,
               Faction   => Faction,
               Statesman =>
                  Agrippa.Cards.Statesmen.Statesman_Card_Type'Class (Card)
               .Statesman);

         when War_Card =>
            declare
               use Agrippa.Cards.Wars;
               W : War_Card_Type'Class renames
                     War_Card_Type'Class (Card);
               Id : constant War_Id := W.War;
            begin
               Game.War_State (Id).On_Drawn;
            end;
      end case;
   end Play_Card;

   --------------------
   -- Play_Statesman --
   --------------------

   procedure Play_Statesman
     (Game      : in out Game_Type'Class;
      Faction   : Faction_Id;
      Statesman : Statesman_Id)
   is
      Card : constant Agrippa.Cards.Statesmen.Statesman_Card_Type'Class :=
               Agrippa.Cards.Statesmen.Statesman (Statesman);
      State : Agrippa.State.Senators.Senator_State_Type renames
                Game.Senator_State (Card.Family);
   begin
      State.Set_Statesman (Faction, Statesman);
   end Play_Statesman;

   -------------------
   -- Recall_Forces --
   -------------------

   procedure Recall_Forces
     (Game : in out Game_Type'Class;
      War  : War_Id)
   is
   begin
      for Legion of Game.Legion_State loop
         if Legion.Deployed
           and then Legion.War = War
         then
            Legion.Recall;
         end if;
      end loop;

      for Fleet of Game.Fleet_State loop
         if Fleet.Deployed
           and then Fleet.War = War
         then
            Fleet.Recall;
         end if;
      end loop;

   end Recall_Forces;

   --------------------------
   -- Regular_Legion_Count --
   --------------------------

   function Regular_Legion_Count
     (Game : Game_Type'Class)
      return Legion_Count
   is
   begin
      return Game.Total_Legion_Count - Game.Veteran_Legion_Count;
   end Regular_Legion_Count;

   ------------------
   -- Senator_Name --
   ------------------

   overriding function Senator_Name
     (Game    : Game_Type;
      Senator : Senator_Id)
      return String
   is
      pragma Unreferenced (Game);
      function To_Name (Tag : String) return String;

      -------------
      -- To_Name --
      -------------

      function To_Name (Tag : String) return String is
      begin
         return Name : String := Tag do
            Name (Name'First) :=
              Ada.Characters.Handling.To_Upper (Name (Name'First));
         end return;
      end To_Name;

   begin
      return To_Name
        (Agrippa.Cards.Senators.Senator (Senator).Tag);
   end Senator_Name;

   ----------------------
   -- Senators_In_Rome --
   ----------------------

   overriding function Senators_In_Rome
     (Game    : Game_Type)
      return Senator_Id_Array
   is
      Result : Senator_Id_Array (1 .. Max_Senators);
      Count  : Natural := 0;
   begin
      for S of Game.Senator_State loop
         if S.In_Rome then
            Count := Count + 1;
            Result (Count) := S.Id;
         end if;
      end loop;
      return Result (1 .. Count);
   end Senators_In_Rome;

   ------------------
   -- Send_Message --
   ------------------

   overriding function Send_Message
     (Game    : in out Game_Type;
      Message : Agrippa.Messages.Message_Type)
     return Agrippa.Messages.Message_Type
   is
      use Agrippa.Messages;

      function To_Player
        (M : Agrippa.Messages.Message_Type)
         return Agrippa.Messages.Message_Type;

      ---------------
      -- To_Player --
      ---------------

      function To_Player
        (M : Agrippa.Messages.Message_Type)
         return Agrippa.Messages.Message_Type
      is
         Player : constant Agrippa.Players.Player_Access :=
                    Game.Player_State (Get_Faction (M)).Handler;
      begin
         Player.Send_Message (Game, M);
         return Response : Message_Type do
            Player.Get_Reply (Response);
         end return;
      end To_Player;

      Result : Agrippa.Messages.Message_Type;

   begin
      case Message.Content is
         when Empty_Message =>
            null;
         when Mortality_Roll =>
            Game.Mortality_Roll;

         when Faction_Transfers =>

            declare
               Faction  : constant Faction_Id := Get_Faction (Message);
               Response : constant Agrippa.Messages.Message_Type :=
                            To_Player (Message);

               procedure Apply_Faction_Treasury
                 (Take   : Boolean;
                  Amount : Talents);

               procedure Apply_Senator_Treasury
                 (Senator : Senator_Id;
                  Take    : Boolean;
                  Amount  : Talents);

               ----------------------------
               -- Apply_Faction_Treasury --
               ----------------------------

               procedure Apply_Faction_Treasury
                 (Take   : Boolean;
                  Amount : Talents)
               is
                  State : Agrippa.Factions.Faction_Type renames
                            Game.Faction_State (Faction);
               begin
                  if Take then
                     State.Set_Treasury (State.Treasury - Amount);
                  else
                     State.Set_Treasury (State.Treasury + Amount);
                  end if;
               end Apply_Faction_Treasury;

               ----------------------------
               -- Apply_Senator_Treasury --
               ----------------------------

               procedure Apply_Senator_Treasury
                 (Senator : Senator_Id;
                  Take    : Boolean;
                  Amount  : Talents)
               is
                  State : Agrippa.State.Senators.Senator_State_Type renames
                            Game.Senator_State (Senator);
               begin
                  if Take then
                     State.Set_Treasury (State.Treasury - Amount);
                  else
                     State.Set_Treasury (State.Treasury + Amount);
                  end if;
               end Apply_Senator_Treasury;

            begin
               Game.Notifier.Send_Message (Game, Response);
               Scan_Transfers (Response,
                               Apply_Faction_Treasury'Access,
                               Apply_Senator_Treasury'Access);
            end;

         when Initiative_Roll =>
            declare
               Roll : constant DR_Range :=
                        Agrippa.Dice.DR;
            begin
               if Roll = 7 then
                  declare
                     Event_Roll : constant TDR_Range :=
                                    Agrippa.Dice.TDR;
                  begin
                     Game.Notifier.Send_Message
                       (Game,
                        Agrippa.Messages.Initiative_Roll
                          (Message, Roll, Event_Roll));
                     Execute_Event (Game, Game.Events (Event_Roll));
                  end;
               else
                  declare
                     Card : constant Agrippa.Cards.Card_Type'Class :=
                              Game.Forum_Deck.Draw;
                  begin

                     if Card.Keep then
                        Game.Faction_State (Get_Faction (Message))
                          .Add_Card (Card);
                     else
                        Game.Play_Card
                          (Faction => Get_Faction (Message),
                           Senator => (if Has_Senator (Message)
                                       then Get_Senator (Message)
                                       else Senator_Id'First),
                           Card    => Card);
                     end if;

                     Game.Notify
                       (Agrippa.Messages.Initiative_Roll
                          (Message, Roll, Card.Id));
                  end;
               end if;
            end;

         when Persuasion_Attempt =>
            declare
               Response : constant Agrippa.Messages.Message_Type :=
                            To_Player (Message);
            begin
               if Response in Persuasion_Attempt_Message then
                  declare
                     Spend    : constant Talents := Get_Money (Response);
                     Senator  : constant Senator_Id := Get_Senator (Response);
                     Target   : constant Senator_Id :=
                                  Get_Persuasion_Target (Response);
                     Roll     : constant DR_Range :=
                                  Agrippa.Dice.DR;
                     Ora      : constant Natural :=
                                  Natural (Game.Oratory (Senator));
                     Inf      : constant Natural :=
                                  Natural (Game.Influence (Senator));
                     Loy      : constant Natural :=
                                  Natural (Game.Loyalty (Target));
                     Tre      : constant Natural :=
                                  Natural (Game.Senator_Treasury (Target));
                     Base     : constant Integer :=
                                  Ora + Inf + Natural (Spend) - Loy - Tre;
                  begin
                     Game.Notifier.Send_Message (Game, Response);
                     Game.Notifier.Send_Notification
                       ("Base = "
                        & Agrippa.Images.Image (Ora)
                        & " Ora + "
                        & Agrippa.Images.Image (Inf)
                        & " Inf + "
                        & Agrippa.Images.Image (Spend)
                        & "t"
                        & " - "
                        & Agrippa.Images.Image (Loy)
                        & " Loy - "
                        & Agrippa.Images.Image (Tre)
                        & "t"
                        & " = " & Agrippa.Images.Image (Base));

                     declare
                        Success  : constant Boolean := Roll <= Base;
                     begin
                        Game.Notifier.Send_Notification
                          ("Base = "
                           & Agrippa.Images.Image (Base)
                           & "; roll of "
                           & Agrippa.Images.Image (Roll)
                           & "; "
                           & (if Success
                             then "Succeeds! "
                             & Game.Senator_Name (Target)
                             & " joins faction "
                             & Game.Faction_Name
                               (Get_Faction (Message))
                             else "Fails! "
                             & Game.Senator_Name (Target)
                             & " remains "
                             & (if Game.Has_Faction (Target)
                               then " with "
                               & Game.Faction_Name
                                 (Game.Senator_Faction (Target))
                               else " unaligned")));
                        Game.Senator_State (Target).Add_Talents (Spend);
                        Game.Senator_State (Senator).Remove_Talents (Spend);
                        if Success then
                           Game.Senator_State (Target).Set_Faction
                             (Get_Faction (Response));
                        end if;
                     end;
                  end;
               end if;
            end;

         when Attract_Knights =>
            declare
               Response : constant Agrippa.Messages.Message_Type :=
                            To_Player (Message);
               Spend    : constant Talents := Get_Money (Response);
               Senator  : constant Senator_Id := Get_Senator (Response);
               Roll     : constant Die_Range :=
                            Agrippa.Dice.Roll_Die;
               Success  : constant Boolean :=
                            Roll + Natural (Spend) >= 6;
            begin
               if Success then
                  Game.Senator_State (Senator).Add_Knight;
               end if;

               Game.Notifier.Send_Message
                 (Game,
                  Attract_Knights
                    (Response, Roll, Success));
            end;

         when Population_Roll =>

            declare
               Response          : Agrippa.Messages.Message_Type := Message;
               Roll              : constant TDR_Range :=
                                     Agrippa.Dice.TDR;
               HRAO              : constant Senator_Id :=
                                     Game.Highest_Ranking_Available_Officer;
               HRAO_Popularity   : constant Popularity_Range :=
                                     Game.Senator_State (HRAO).Popularity;
               Unrest            : Natural := Natural (Game.Unrest);

               function Unprosecuted (War : War_Id) return Boolean;

               ------------------
               -- Unprosecuted --
               ------------------

               function Unprosecuted (War : War_Id) return Boolean is
                  W : Agrippa.State.Wars.War_State_Type renames
                        Game.War_State.Element (War);
               begin
                  return W.Active and then W.Unprosecuted;
               end Unprosecuted;

            begin
               Add_Table_Row (Response, "current-unrest-level", Unrest);
               for Id of Game.Matching_Wars (Unprosecuted'Access) loop
                  Add_Table_Row
                    (Message => Response,
                     Heading => "unrest-level-raised-by",
                     Value   => 1,
                     Comment =>
                       Game.Local_Text
                         ("unprosecuted-war",
                          Game.Local_Text
                            (Agrippa.Cards.Wars.War (Id).Tag)));
                  Unrest := Unrest + 1;
               end loop;

               Add_Table_Row (Response, "hrao-popularity",
                              Integer (HRAO_Popularity));
               Game.Unrest := Unrest_Level (Unrest);

               declare
                  use Agrippa.Images;
                  Final : constant Integer :=
                            Roll - Unrest
                              + Integer (HRAO_Popularity);
               begin
                  Add_Table_Row
                    (Message => Response,
                     Heading => "population-roll",
                     Value   =>
                       Image (Roll)
                     & " - "
                     & Image (Unrest)
                     & " "
                     & (if HRAO_Popularity < 0 then "-" else "+")
                     & " "
                     & Image (abs HRAO_Popularity)
                     & " = "
                     & Image (Final));
                  Game.Apply_Population_Roll (Final, Response);

               end;

               Game.Notifier.Send_Message (Game, Response);

               if not Has_Property_Updates (Response) then
                  Game.Notifier.Send_Notification
                    (Game.Local_Text ("no-change"));
               end if;
            end;

         when Make_Proposal =>

            declare
               Response : constant Message_Type :=
                            To_Player (Message);
            begin

               if Agrippa.Proposals.Is_Empty
                 (Agrippa.Messages.Proposals (Response))
               then
                  Game.Send_Text_Notification
                    (Game.Local_Text
                       ("x-adjourns-the-senate",
                        Game.Senator_Name
                          (Game.Highest_Ranking_Available_Officer)));
                  Game.Senate_Adjourned := True;
               else
                  Game.Notifier.Send_Message (Game, Response);

                  Game.Hold_Vote
                    (Agrippa.Messages.Get_Senator (Response),
                     Agrippa.Messages.Proposals (Response));

               end if;
            end;

         when Proposal_Vote =>

            Game.Notifier.Send_Message (Game, Message);

         when Attack =>
            declare
               War : constant War_Id := Agrippa.Messages.War (Message);
            begin
               Game.Notifier.Send_Message
                 (State   => Game,
                  Message =>
                    Agrippa.Messages.Attack_With
                      (Message   => Message,
                       Commander => Game.War_Commander (War),
                       Legions   => Game.Deployed_Legions (War),
                       Fleets    => Game.Deployed_Fleets (War)));
               Game.Attack (War);
            end;

         when Player_Action =>

            Result := To_Player (Message);

            if Result.Content = Player_Action then
               Game.Notifier.Send_Message (Game, Result);
               case Get_Action (Result) is
                  when Check_Rebellion =>
                     declare
                        Senator : constant Senator_Id :=
                                    Get_Senator (Result);
                        War     : constant War_Id :=
                                    Game.Senator_State (Senator).Command;
                     begin
                        if Rebels (Result) then
                           Game.Senator_State (Senator).Rebel;
                        else
                           Game.Recall_Forces (War);
                           Game.Senator_State (Senator).Return_To_Rome;
                        end if;
                     end;

                  when Play_Card =>
                     declare
                        Card : constant Agrippa.Cards.Card_Type'Class :=
                                 Agrippa.Cards.Card (Get_Card (Result));
                     begin
                        Game.Faction_State (Get_Faction (Result)).Remove_Card
                          (Card);
                        Game.Play_Card
                          (Get_Faction (Result), Get_Senator (Result), Card);
                     end;
               end case;
            end if;

      end case;

      return Result;

   end Send_Message;

   ----------------------------
   -- Send_Text_Notification --
   ----------------------------

   overriding procedure Send_Text_Notification
     (Game  : Game_Type;
      Text  : String)
   is
   begin
      Game.Notifier.Send_Notification (Text);
   end Send_Text_Notification;

   --------------------
   -- Set_Autoplayer --
   --------------------

   procedure Set_Autoplayer
     (Game    : in out Game_Type'Class;
      Faction : Faction_Id;
      Player  : Agrippa.Players.Autoplayer_Interface'Class)
   is
   begin
      Game.Player_State (Faction).Autoplayer :=
        Autoplayer_Holders.To_Holder (Player);
   end Set_Autoplayer;

   --------------------------
   -- Set_Current_Activity --
   --------------------------

   procedure Set_Current_Activity
     (Game  : in out Game_Type'Class;
      Phase : Natural;
      Step  : Natural)
   is
   begin
      Game.Phase_Number := Phase;
      Game.Step_Number := Step;
   end Set_Current_Activity;

   ------------------------
   -- Set_Faction_Leader --
   ------------------------

   overriding procedure Set_Faction_Leader
     (Game    : in out Game_Type;
      Faction : Faction_Id;
      Senator : Senator_Id)
   is
   begin
      Game.Faction_State (Faction).Set_Leader (Senator);
      Game.Notifier.On_Faction_Leader_Changed (Game, Faction);
   end Set_Faction_Leader;

   ----------------
   -- Set_Office --
   ----------------

   overriding procedure Set_Office
     (Game    : in out Game_Type;
      Senator : Senator_Id;
      Office  : Office_Type)
   is
      State : Agrippa.State.Senators.Senator_State_Type renames
                Game.Senator_State (Senator);
   begin
      if Game.Has_Holder (Office) then
         Game.Senator_State (Game.Office_Holder (Office)).Clear_Office;
      end if;

      State.Set_Office (Office);

      if Office = Rome_Consul or else Office = Field_Consul then
         Game.Notifier.Send_Notification
           (Game.Local_Text
              ("x-selects-office",
               Game.Senator_Name (Senator),
               Game.Local_Text (Office'Image)));
      else
         Game.Notifier.Send_Notification
           (Game.Local_Text
              ("x-is-the-new-office-holder",
               Game.Senator_Name_And_Faction (Senator),
               Game.Local_Text (Office'Image)));
      end if;

   end Set_Office;

   ----------------
   -- Set_Player --
   ----------------

   procedure Set_Player
     (Game    : in out Game_Type'Class;
      Faction : Faction_Id;
      Player  : Agrippa.Players.Player_Access)
   is
   begin
      Game.Player_State (Faction).Handler := Player;
   end Set_Player;

   -------------------------
   -- Set_Player_Handlers --
   -------------------------

   procedure Set_Player_Handlers
     (Game    : in out Game_Type'Class)
   is
      Handlers : Agrippa.Players.Player_Access_Array;
   begin
      for Faction in Faction_Id loop
         Handlers (Faction) := Game.Player_State (Faction).Handler;
      end loop;

      for Faction in Faction_Id loop
         if not Game.Player_State (Faction).Autoplayer.Is_Empty then
            Game.Player_State (Faction).Autoplayer.Reference.Set_Players
              (Handlers);
         end if;
      end loop;
   end Set_Player_Handlers;

   ------------------
   -- Set_Treasury --
   ------------------

   procedure Set_Treasury
     (Game         : in out Game_Type'Class;
      New_Treasury : Talents)
   is
   begin
      Game.Treasury := New_Treasury;
   end Set_Treasury;

   ----------------------
   -- Set_Unprosecuted --
   ----------------------

   overriding procedure Set_Unprosecuted
     (Game : in out Game_Type;
      War  : War_Id)
   is
   begin
      Game.War_State (War).Unprosecuted;
   end Set_Unprosecuted;

   -----------
   -- Start --
   -----------

   procedure Start
     (Game     : in out Game_Type'Class;
      Scenario : Agrippa.Scenarios.Scenario_Type;
      Language : WL.Localisation.Language_Type;
      Notify   : not null access constant
        Agrippa.State.Notifications.Change_Handler_Interface'Class)
   is
      package Senator_Id_Vectors is
        new Ada.Containers.Vectors (Positive, Senator_Id);

      Senator_Ids : Senator_Id_Vectors.Vector;
   begin
      Game.Scenario := Scenario;
      Game.Language := Language;
      Game.Notifier := Notify;

      declare
         use all type Agrippa.Events.Event_Type;
      begin
         Game.Events :=
           (3 => Mob_Violence,
            4 => Natural_Disaster,
            5 => Ally_Deserts,
            6 => Evil_Omens,
            7 => Refuge,
            8 => Epidemic,
            9 => Drought,
            10 => Evil_Omens,
            11 => Storm_At_Sea,
            12 => Manpower_Shortage,
            13 => Allied_Enthusiasm,
            14 => New_Alliance,
            15 => Rhodian_Alliance,
            16 => Enemy_Ally_Deserts,
            17 => Enemy_Leader_Dies,
            18 => Trial_Of_Verres);
      end;

      for Id of Agrippa.Cards.Senators.All_Senators loop
         Game.Senator_State (Id).Set_Id (Id);
         if Agrippa.Scenarios.Includes
           (Game.Scenario,
            Agrippa.Cards.Senators.Senator (Id).Scenario)
         then
            Game.Senator_State (Id).Set_In_Play;
            Senator_Ids.Append (Id);
         end if;
      end loop;

      for Faction of Game.Faction_State loop
         if Faction.Active then
            for I in 1 .. 3 loop
               declare
                  Index : constant Positive :=
                            WL.Random.Random_Number
                              (1, Senator_Ids.Last_Index);
                  Id    : constant Senator_Id :=
                            Senator_Ids.Element (Index);
               begin
                  Game.Senator_State (Id).Set_Faction (Faction.Id);
                  if Index < Senator_Ids.Last_Index then
                     Senator_Ids.Replace_Element
                       (Index, Senator_Ids.Last_Element);
                  end if;
                  Senator_Ids.Delete_Last;
               end;
            end loop;
         end if;
      end loop;

      for I in Legion_Index range 1 .. 4 loop
         Game.Legion_State (I).Create;
      end loop;

      declare
         RC : constant Senator_Id :=
                Game.Highest_Ranking_Available_Officer;
      begin
         Game.Senator_State (RC).Set_Office (Rome_Consul);
      end;

      for Id of Agrippa.Cards.Statesmen.All_Statesmen (Game.Scenario) loop
         Game.Forum_Deck.Add
           (Agrippa.Cards.Statesmen.Statesman (Id));
      end loop;

      for Id of Agrippa.Cards.Concessions.All_Concessions (Game.Scenario) loop
         Game.Forum_Deck.Add
           (Agrippa.Cards.Concessions.Concession (Id));
      end loop;

      for Id of Agrippa.Cards.Intrigue.All_Intrigues (Game.Scenario) loop
         Game.Forum_Deck.Add
           (Agrippa.Cards.Intrigue.Intrigue (Id));
      end loop;

      for Faction of Game.Faction_State loop
         if Faction.Active then
            Ada.Text_IO.Put (Faction.Name
                             & " draws");
            for I in 1 .. 3 loop
               declare
                  Card : constant Agrippa.Cards.Card_Type'Class :=
                           Game.Forum_Deck.Draw;
               begin
                  Ada.Text_IO.Put (" " & Card.Tag);
                  Faction.Add_Card (Card);
               end;
            end loop;
            Ada.Text_IO.New_Line;
         end if;
      end loop;

      for Id of Senator_Ids loop
         if not Game.Senator_State (Id).Has_Faction then
            Agrippa.Cards.Decks.Add
              (Game.Forum_Deck,
               Agrippa.Cards.Senators.Senator (Id));
         end if;
      end loop;

      for Id of Agrippa.Cards.Wars.All_Wars loop
         declare
            Card : constant Agrippa.Cards.Wars.War_Card_Type'Class :=
                     Agrippa.Cards.Wars.War (Id);
            In_Play : constant Boolean :=
                        Agrippa.Scenarios.Includes
                          (Game.Scenario, Card.Scenario);
            WS : Agrippa.State.Wars.War_State_Type;
         begin
            WS.Initialize (Id, In_Play);
            Game.War_State.Append (WS);
            if Card.Tag = "first-punic-war" then
               Game.Play_Card (Faction_Id'First, Senator_Id'First, Card);
            else
               Game.Forum_Deck.Add (Card);
            end if;
         end;
      end loop;

      for Id of Agrippa.Cards.Leaders.All_Leaders (Game.Scenario) loop
         declare
            Card    : constant Agrippa.Cards.Leaders.Leader_Card_Type'Class :=
                        Agrippa.Cards.Leaders.Leader (Id);
         begin
            Game.Leader_State (Id).Set_In_Play (Id);
            Game.Forum_Deck.Add (Card);
         end;
      end loop;

      Game.Current_Turn := 0;

   end Start;

   ------------------
   -- Start_Combat --
   ------------------

   overriding procedure Start_Combat
     (Game    : in out Game_Type)
   is null;

   --------------------------
   -- Start_Senate_Session --
   --------------------------

   overriding procedure Start_Senate_Session
     (Game    : in out Game_Type)
   is
      use Ada.Strings.Unbounded;
      Total_Votes : Vote_Count := 0;
      Message     : Unbounded_String;
   begin
      for War of Game.War_State loop
         if War.Imminent then
            War.Activate;
         end if;
      end loop;

      for Faction in Faction_Id loop
         declare
            Votes : constant Vote_Count :=
                      Game.Faction_Votes (Faction);
         begin
            Message := Message
              & (if Faction = 1 then "" else " ")
              & Game.Faction_Name (Faction) (1 .. 3)
              & ":"
              & Agrippa.Images.Image (Votes);
            Total_Votes := Total_Votes + Votes;
         end;
      end loop;

      Game.Notifier.Send_Notification
        ("***"
         & Game.Local_Text ("votes-available")
         & " = "
         & Agrippa.Images.Image (Total_Votes)
         & " ("
         & To_String (Message)
         & ")");

      declare
         HRAO : constant Senator_Id :=
                  Game.Highest_Ranking_Available_Officer;
         Faction : constant Faction_Id :=
                     Game.Senator_Faction (HRAO);
      begin
         Game.Notifier.Send_Notification
           (Game.Local_Text
              ("x-is-the-presiding-magistrate",
               Game.Local_Text
                 ("senator-of-faction",
                  Game.Senator_Name (HRAO),
                  Game.Faction_Name (Faction))));
      end;

   end Start_Senate_Session;

   ----------------
   -- Start_Turn --
   ----------------

   procedure Start_Turn
     (Game : in out Game_Type'Class)
   is
   begin
      Game.Current_Turn := Game.Current_Turn + 1;
      for Player of Game.Player_State loop
         Player.Handler.Start_Turn (Game);
      end loop;
   end Start_Turn;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Game     : in out Game_Type'Class)
   is
      use type Agrippa.Players.Player_Access;
   begin
      for Player of Game.Player_State loop
         if Player.Handler /= null then
            Player.Handler.Stop;
         end if;
      end loop;
   end Stop;

   -----------------------
   -- Total_Fleet_Count --
   -----------------------

   overriding function Total_Fleet_Count
     (Game : Game_Type)
      return Fleet_Count
   is
   begin
      return Count : Fleet_Count := 0 do
         for Fleet of Game.Fleet_State loop
            if Fleet.Created then
               Count := Count + 1;
            end if;
         end loop;
      end return;
   end Total_Fleet_Count;

   ------------------------
   -- Total_Legion_Count --
   ------------------------

   overriding function Total_Legion_Count
     (Game : Game_Type)
      return Legion_Count
   is
   begin
      return Game.Count_Legions
        (Agrippa.State.Legions.Created'Access);
   end Total_Legion_Count;

   --------------------------
   -- Veteran_Legion_Count --
   --------------------------

   function Veteran_Legion_Count
     (Game : Game_Type'Class)
      return Legion_Count
   is
   begin
      return Game.Count_Legions
        (Agrippa.State.Legions.Veteran'Access);
   end Veteran_Legion_Count;

   -------------------
   -- War_Commander --
   -------------------

   function War_Commander
     (Game : Game_Type'Class;
      War  : War_Id)
      return Senator_Id
   is
   begin
      for Id in Game.Senator_State'Range loop
         if Game.Senator_State (Id).Has_Command
           and then Game.Senator_State (Id).Command = War
         then
            return Id;
         end if;
      end loop;
      raise Constraint_Error with
        "no commander for "
        & Game.Local_Text
        (Agrippa.Cards.Wars.War (War).Tag);
   end War_Commander;

end Agrippa.Game;
