with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Agrippa.Dice;
with Agrippa.Proposals;

with Agrippa.Cards.Senators;
with Agrippa.Cards.Statesmen;
with Agrippa.Cards.Wars;

with Agrippa.Players.Autohandler;

package body Agrippa.Players.Robots is

   type Senator_Record is
      record
         Senator    : Senator_Id;
         Votes      : Vote_Count;
         Influence  : Influence_Range;
         Popularity : Popularity_Range;
         Military   : Attribute_Range;
      end record;

   package Senator_Vectors is
     new Ada.Containers.Vectors (Positive, Senator_Record);

   package Offer_Vectors is
     new Ada.Containers.Vectors (Positive, Agrippa.Deals.Offer_Type,
                                 Agrippa.Deals."=");

   type Faction_Record is
      record
         Faction   : Faction_Id;
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         Votes     : Vote_Count;
         Influence : Faction_Influence_Range;
         Senators  : Senator_Vectors.Vector;
         Desire    : Offer_Vectors.Vector;
      end record;

   function Create
     (State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
      return Faction_Record;

   package Faction_Vectors is
     new Ada.Containers.Vectors (Positive, Faction_Record);

   function More_Votes (Left, Right : Faction_Record) return Boolean is
     (Left.Votes > Right.Votes
      or else (Left.Votes = Right.Votes
               and then Left.Influence > Right.Influence));

   package Faction_Sorting is
     new Faction_Vectors.Generic_Sorting (More_Votes);

   type Coalition_Type is
      record
         Index  : Natural;
         Vector : Faction_Vectors.Vector;
      end record;

   function Show (Coalition : Coalition_Type) return String;

   type Robot_Player_Type is
     new Autoplayer_Interface with
      record
         Robot_Faction : Robot_Faction_Type;
         Faction       : Faction_Id;
         Handler       : Player_Access;
         Handlers      : Player_Access_Array;
         Current_Deal  : Agrippa.Deals.Deal_Type;
      end record;

   overriding function Name
     (Player : Robot_Player_Type)
      return String
   is (case Player.Robot_Faction is
          when Conservative => "Conservative Faction",
          when Imperial     => "Imperial Faction",
          when Plutocratic  => "Plutocratic Faction",
          when Populist     => "Populist Faction");

   overriding function Get_Player_Handler
     (Robot : Robot_Player_Type)
      return Player_Access
   is (Robot.Handler);

   overriding procedure Initialize
     (Robot   : in out Robot_Player_Type;
      State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id);

   overriding procedure Start_Turn
     (Robot   : Robot_Player_Type;
      State   : in out Agrippa.State.State_Interface'Class);

   overriding function Send_Message
     (Robot   : in out Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
      return Agrippa.Messages.Message_Type;

   overriding function Senate_Phase_Desire
     (Robot   : Robot_Player_Type;
      State  : Agrippa.State.State_Interface'Class)
      return Agrippa.Deals.Offer_List;

   overriding function What_Do_You_Want_For
     (Robot   : Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Offer   : Agrippa.Deals.Offer_List)
      return Agrippa.Deals.Offer_List;

   overriding function What_Will_You_Give_For
     (Robot   : Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Offer   : Agrippa.Deals.Offer_List)
      return Agrippa.Deals.Offer_List;

   overriding function Will_You_Agree_To
     (Robot   : in out Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Deal    : Agrippa.Deals.Deal_Type)
      return Boolean;

   overriding function Vote
     (Player    : Robot_Player_Type;
      State     : Agrippa.State.State_Interface'Class;
      Sponsor   : Senator_Id;
      Proposals : Agrippa.Proposals.Proposal_Container_Type)
      return Faction_Vote_Type;

   overriding procedure Set_Players
     (Robot   : in out Robot_Player_Type;
      Players : Player_Access_Array);

   function Desired_Candidate
     (Robot  : Robot_Player_Type'Class;
      State  : Agrippa.State.State_Interface'Class;
      Office : Office_Type)
      return Nullable_Senator_Id;

   function Desired_Spoils
     (Robot  : Robot_Player_Type'Class;
      State  : Agrippa.State.State_Interface'Class)
      return Agrippa.Deals.Offer_List;

   function Create_Proposal
     (Robot   : in out Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Make_Proposal_Message)
      return Agrippa.Messages.Message_Type;

   function Make_Persuasion_Attempt
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Persuader : out Senator_Id;
      Target    : out Senator_Id;
      Spend     : out Talents)
      return Boolean;

   procedure Attract_Knights
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Senator : out Senator_Id;
      Spend   : out Talents);

   procedure Play_Card
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Senator : out Senator_Id;
      Card    : out Card_Id);

   function Create_Coalition
     (Robot    : Robot_Player_Type'Class;
      State    : Agrippa.State.State_Interface'Class;
      Current  : Natural)
      return Coalition_Type;

   procedure Create_Proposal
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Category  : Agrippa.Proposals.Proposal_Category_Type;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type);

   function Proposal_Matches_Deal
     (Faction  : Faction_Id;
      State    : Agrippa.State.State_Interface'Class;
      Deal     : Agrippa.Deals.Deal_Type;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
      return Boolean;

   function Score_Proposals
     (Faction  : Faction_Id;
      State    : Agrippa.State.State_Interface'Class;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
      return Integer;

   function Attack_War
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      War       : War_Id;
      Minimal   : Boolean;
      Overwhelm : Boolean;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type)
      return Boolean;

   function Create_Election_Deal
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Coalition : Faction_Vectors.Vector)
      return Agrippa.Deals.Deal_Type;

   ----------------
   -- Attack_War --
   ----------------

   function Attack_War
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      War       : War_Id;
      Minimal   : Boolean;
      Overwhelm : Boolean;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type)
      return Boolean
   is
      pragma Unreferenced (Robot, Coalition);
      War_State          : constant Agrippa.State.War_State_Interface'Class :=
                             State.Get_War_State (War);
      Fleet_Strength     : constant Fleet_Count := War_State.Fleet_Strength;
      Fleet_Support      : constant Fleet_Count := War_State.Fleet_Support;
      Fleet_Attack       : constant Boolean :=
                             Fleet_Strength > 0 and then
                                 not War_State.Fleet_Victory;
      Required_Fleets    : constant Fleet_Count :=
                             (if Fleet_Attack
                              then Fleet_Strength + 3
                              else Fleet_Support);
      Deployed_Fleets    : constant Fleet_Index_Array :=
                             State.Deployed_Fleets (War);
      Land_Strength      : constant Combat_Strength :=
                             War_State.Land_Strength;
      Required_Strength  : constant Combat_Strength :=
                             Combat_Strength'Max
                               ((if Overwhelm
                                then Land_Strength + 5
                                elsif Minimal
                                then Land_Strength
                                else Land_Strength + 3),
                                (if Minimal then Land_Strength else 10));
      Veteran_Legions    : constant Legion_Index_Array :=
                             (if Fleet_Attack then No_Legions
                              else State.Available_Veteran_Legions);
      Deployed_Legions   : constant Legion_Index_Array :=
                             State.Deployed_Legions (War);
      Available_Strength : constant Combat_Strength :=
                             State.Legion_Strength (Deployed_Legions)
                             + State.Legion_Strength (Veteran_Legions)
                             + Combat_Strength
                               (State.Available_Regular_Legions);
      Recruit_Legions    : constant Legion_Count :=
                             (if Fleet_Attack
                              or else Available_Strength >= Required_Strength
                              then 0
                              else Legion_Count
                                (Required_Strength - Available_Strength));
      Available_Fleets   : constant Fleet_Count :=
                             State.Available_Fleets + Deployed_Fleets'Length;
      Recruit_Fleets     : constant Fleet_Count :=
                             (if Available_Fleets >= Required_Fleets
                              then 0
                              else Required_Fleets - Available_Fleets);
      Total_Recruitment   : constant Natural :=
                              Natural (Recruit_Legions)
                              + Natural (Recruit_Fleets);
      Max_Recuitment      : constant Natural :=
                              Natural (State.Current_Treasury
                                       / State.Current_Legion_Cost);
      Canceled_Recuitment : constant Natural :=
                              (if Total_Recruitment > Max_Recuitment
                               then Total_Recruitment - Max_Recuitment
                               else 0);
      Final_Recruit_Legions : constant Legion_Count :=
                               (if Canceled_Recuitment = 0
                                then Recruit_Legions
                                elsif Canceled_Recuitment
                                <= Natural (Recruit_Legions)
                                then Recruit_Legions
                                - Legion_Count (Canceled_Recuitment)
                                else 0);
      Final_Recruit_Fleets  : constant Fleet_Count := Recruit_Fleets;
      Final_Strength        : constant Combat_Strength :=
                                Available_Strength
                                  + Combat_Strength (Final_Recruit_Legions);
      Final_Fleets          : constant Fleet_Count :=
                                Available_Fleets + Final_Recruit_Fleets;
      Reinforce             : constant Boolean := State.Has_Commander (War);
      Commander             : constant Senator_Id :=
                                (if State.Has_Commander (War)
                                 then State.Commander (War)
                                 elsif State.Has_Holder (Dictator)
                                 and then not State.Has_Command
                                   (State.Office_Holder (Dictator))
                                 then State.Office_Holder (Dictator)
                                 elsif State.Has_Holder (Field_Consul)
                                 and then not State.Has_Command
                                   (State.Office_Holder (Field_Consul))
                                 then State.Office_Holder (Field_Consul)
                                 elsif State.Has_Holder (Rome_Consul)
                                 and then not State.Has_Command
                                   (State.Office_Holder (Rome_Consul))
                                 then State.Office_Holder (Rome_Consul)
                                 else raise Constraint_Error with
                                   "no available commander");
   begin
      if Fleet_Attack then
         State.Log
           ("attacking " & Agrippa.Cards.Wars.War (War).Tag
            & ": fleet strength" & Fleet_Strength'Image
            & "; required fleets" & Required_Fleets'Image
            & "; available fleets" & Available_Fleets'Image
            & "; required recruitment" & Recruit_Fleets'Image
            & "; max recruitment" & Max_Recuitment'Image);
      else
         State.Log
           ("attacking " & Agrippa.Cards.Wars.War (War).Tag
            & ": land strength" & Land_Strength'Image
            & "; required strength" & Required_Strength'Image
            & "; Available_Strength" & Available_Strength'Image
            & "; required recruitment" & Recruit_Legions'Image
            & "; max recruitment" & Max_Recuitment'Image
            & "; canceled recruitment" & Canceled_Recuitment'Image);
      end if;

      if Canceled_Recuitment > 0 then
         State.Log
           ("Canceling attack because we could not recruit"
            & Canceled_Recuitment'Image
            & " legions");
         return False;
      end if;

      if Fleet_Attack then
         if Final_Fleets < Required_Fleets
           and then not Reinforce
         then
            State.Log
              ("Canceling attack because final fleet count is"
               & Final_Fleets'Image
               & " but we need"
               & Required_Fleets'Image);
            return False;
         end if;
      else
         if Final_Strength < Required_Strength
           and then not Reinforce
         then
            State.Log
              ("Canceling attack because final strength is"
               & Final_Strength'Image
               & " but we need"
               & Required_Strength'Image);
            return False;
         end if;
      end if;

      if Final_Recruit_Legions > 0
        or else Final_Recruit_Fleets > 0
      then
         Agrippa.Proposals.Add_Proposal
           (Container,
            Agrippa.Proposals.Recruit
              (Legions => Final_Recruit_Legions,
               Fleets  => Final_Recruit_Fleets));
      end if;

      if Reinforce then
         Agrippa.Proposals.Add_Proposal
           (Container => Container,
            Proposal  =>
              Agrippa.Proposals.Reinforce
                (War             => War,
                 Regular_Legions =>
                   (if Fleet_Attack then 0
                    else State.Available_Regular_Legions
                    + Final_Recruit_Legions),
                 Veteran_Legions => Veteran_Legions,
                 Fleets          => Required_Fleets));
      else
         Agrippa.Proposals.Add_Proposal
           (Container => Container,
            Proposal  =>
              Agrippa.Proposals.Attack
                (War             => War,
                 Commander       => Commander,
                 Regular_Legions =>
                   (if Fleet_Attack then 0
                    else State.Available_Regular_Legions
                    + Final_Recruit_Legions),
                 Veteran_Legions => Veteran_Legions,
                 Fleets          => Required_Fleets));
      end if;
      return True;

   end Attack_War;

   ---------------------
   -- Attract_Knights --
   ---------------------

   procedure Attract_Knights
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Senator : out Senator_Id;
      Spend   : out Talents)
   is
      Max_Spend : constant Talents :=
                    (case Robot.Robot_Faction is
                        when Conservative => 0,
                        when Imperial     => 1,
                        when Plutocratic  => 2,
                        when Populist     => 1);
   begin
      Spend :=
        Talents'Min (Max_Spend,
                     State.Faction_Treasury (Robot.Faction));
      Senator := State.Faction_Leader (Robot.Faction);
   end Attract_Knights;

   ------------
   -- Create --
   ------------

   function Create
     (State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
      return Faction_Record
   is
   begin
      return Rec : Faction_Record := Faction_Record'
        (Faction   => Faction,
         Name      =>
           Ada.Strings.Unbounded.To_Unbounded_String
             (State.Faction_Name (Faction)),
         Votes     => State.Faction_Votes (Faction),
         Influence => State.Faction_Influence (Faction),
         Senators  => <>,
         Desire    => <>)
      do
         for Senator of State.Faction_Senators (Faction) loop
            Rec.Senators.Append
              (Senator_Record'
                 (Senator    => Senator,
                  Votes      => State.Senator_Votes (Senator),
                  Influence  => State.Influence (Senator),
                  Popularity => State.Popularity (Senator),
                  Military   => State.Military (Senator)));
         end loop;
      end return;
   end Create;

   ----------------------
   -- Create_Coalition --
   ----------------------

   function Create_Coalition
     (Robot    : Robot_Player_Type'Class;
      State    : Agrippa.State.State_Interface'Class;
      Current  : Natural)
      return Coalition_Type
   is
      Total_Votes    : Vote_Count := 0;
      Required_Votes : Vote_Count;
      This_Faction   : constant Faction_Record :=
                         Create (State, Robot.Faction);
      Other_Factions : Faction_Vectors.Vector;

      function Check_Coalition
        (I, J, K : Natural)
         return Boolean;

      function Check_Coalition
        (I, J, K : Natural)
         return Boolean
      is
         Coalition_Votes : Vote_Count := 0;

         procedure Add (Index : Natural);

         ---------
         -- Add --
         ---------

         procedure Add (Index : Natural) is
         begin
            if Index > 0 then
               Coalition_Votes := Coalition_Votes
                 + Other_Factions.Element (Index).Votes;
            end if;
         end Add;

      begin
         Add (I);
         Add (J);
         Add (K);

         return Coalition_Votes >= Required_Votes;
      end Check_Coalition;

   begin

      for Faction in Faction_Id loop
         Total_Votes := Total_Votes + State.Faction_Votes (Faction);
         if Faction /= Robot.Faction then
            Other_Factions.Append (Create (State, Faction));
         end if;
      end loop;
      Required_Votes :=
        Total_Votes / 2 + 1 - State.Faction_Votes (Robot.Faction);

      Faction_Sorting.Sort (Other_Factions);

      declare
         Coalitions : constant array (1 .. 13, 1 .. 3)
           of Natural :=
             ((1, 0, 0),
              (2, 5, 0),
              (2, 4, 0),
              (2, 3, 0),
              (1, 5, 0),
              (1, 4, 0),
              (1, 3, 0),
              (1, 2, 0),
              (3, 4, 5),
              (2, 3, 4),
              (1, 2, 5),
              (1, 2, 4),
              (1, 2, 3));

      begin
         for Trial_Index in Current + 1 .. Coalitions'Last (1) loop
            declare
               X : constant Natural := Coalitions (Trial_Index, 1);
               Y : constant Natural := Coalitions (Trial_Index, 2);
               Z : constant Natural := Coalitions (Trial_Index, 3);
            begin
               if Check_Coalition (X, Y, Z) then
                  return Result : Coalition_Type do
                     Result.Index := Trial_Index;
                     Result.Vector.Append (This_Faction);

                     declare
                        procedure Add (Index : Natural);

                        ---------
                        -- Add --
                        ---------

                        procedure Add (Index : Natural) is
                        begin
                           if Index > 0 then
                              Result.Vector.Append
                                (Other_Factions.Element (Index));
                           end if;
                        end Add;

                     begin
                        Add (X);
                        Add (Y);
                        Add (Z);
                     end;

                     Faction_Sorting.Sort (Result.Vector);
                  end return;
               end if;
            end;
         end loop;
      end;

      return Result : Coalition_Type do
         Result.Index := 0;
         Result.Vector.Append (This_Faction);
         Result.Vector.Append (Other_Factions.Element (2));
         Result.Vector.Append (Other_Factions.Element (3));
         Result.Vector.Append (Other_Factions.Element (4));
         Result.Vector.Append (Other_Factions.Element (5));
      end return;
   end Create_Coalition;

   --------------------------
   -- Create_Election_Deal --
   --------------------------

   function Create_Election_Deal
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Coalition : Faction_Vectors.Vector)
      return Agrippa.Deals.Deal_Type
   is
      pragma Unreferenced (Robot);
      use Agrippa.Deals;
      Deal      : Deal_Type;
      Allocated : array (Office_Type) of Boolean := (others => False);
      Success   : Boolean := False;

      type Assignment is array (Office_Type) of Nullable_Senator_Id;
      package Assignment_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Assignment);

      Tried_List  : Assignment_Lists.List;
      Current_Try : Assignment := (others => No_Senator);

      Nominated   : array (Senator_Id) of Boolean :=
                      (others => False);

      function Tried
        (Current : Deal_Type;
         Office  : Office_Type;
         Senator : Senator_Id)
         return Boolean;

      function Coalition_Member (Faction : Faction_Id) return Boolean
      is (for some Rec of Coalition => Rec.Faction = Faction);

      -----------
      -- Tried --
      -----------

      function Tried
        (Current : Deal_Type;
         Office  : Office_Type;
         Senator : Senator_Id)
         return Boolean
      is
         Found : Boolean := False;
      begin
         for Assignment of Tried_List loop
            if Has_Senator (Assignment (Office))
              and then To_Senator_Id (Assignment (Office)) = Senator
            then
               Found := True;

               declare

                  Checked : array (Office_Type) of Boolean :=
                              (others => False);

                  procedure Check
                    (Faction : Faction_Id;
                     Offer   : Offer_Type);

                  -----------
                  -- Check --
                  -----------

                  procedure Check
                    (Faction : Faction_Id;
                     Offer   : Offer_Type)
                  is
                     pragma Unreferenced (Faction);
                  begin
                     if Is_Office_Offer (Offer) then
                        Checked (Get_Office (Offer)) := True;
                     end if;

                     if Is_Office_Offer (Offer)
                       and then Assignment (Get_Office (Offer))
                         /= To_Nullable_Id (Get_Holder (Offer))
                     then
                        Found := False;
                     end if;
                  end Check;

               begin
                  Checked (Office) := True;
                  Scan (Current, Check'Access);

                  if Found then
                     for Check_Office in Checked'Range loop
                        if not Checked (Check_Office)
                          and then Assignment (Check_Office) /= No_Senator
                        then
                           Found := False;
                           exit;
                        end if;
                     end loop;
                  end if;
               end;
            end if;
            exit when Found;
         end loop;
         return Found;
      end Tried;

   begin

      for Retry in Boolean loop

         Tried_List.Clear;

         while not Success loop

            State.Log ("negotiating a deal");

            Allocated := (others => False);
            Current_Try := (others => No_Senator);
            Nominated := (others => False);

            Clear (Deal);

            for Rec of Coalition loop

               Success := False;

               for Choice of Rec.Desire loop
                  if Is_Office_Offer (Choice) then
                     declare
                        Office : constant Office_Type :=
                                   Agrippa.Deals.Get_Office (Choice);
                        Holder : constant Senator_Id :=
                                   Agrippa.Deals.Get_Holder (Choice);
                     begin
                        if Allocated (Office) then
                           State.Log
                             (State.Faction_Name (Rec.Faction)
                              & ": skipping "
                              & Office'Image);
                        elsif not Retry
                          and then State.Senator_Faction (Holder)
                          /= Rec.Faction
                        then
                           State.Log
                             (State.Faction_Name (Rec.Faction)
                              & ": not my office "
                              & Office'Image);
                        elsif Retry
                          and then State.Senator_Faction (Holder)
                          /= Rec.Faction
                          and then Coalition_Member
                            (State.Senator_Faction (Holder))
                        then
                           State.Log
                             (State.Faction_Name (Rec.Faction)
                              & ": not proposing "
                              & State.Senator_Name_And_Faction (Holder)
                              & " for "
                              & Office'Image
                              & " because my coalition partner can do it");
                        elsif Tried (Deal, Office, Holder) then
                           declare
                              Report_Deal : Deal_Type := Deal;
                           begin
                              Add (Report_Deal, Rec.Faction, Choice);
                              State.Log
                                (State.Faction_Name (Rec.Faction)
                                 & ": already tried "
                                 & Show (Report_Deal, State));
                           end;
                        end if;

                        if not Allocated (Office)
                          and then (Retry
                                    or else State.Senator_Faction (Holder)
                                    = Rec.Faction)
                          and then (not Retry
                                    or else State.Senator_Faction (Holder)
                                    = Rec.Faction
                                    or else not Coalition_Member
                                      (State.Senator_Faction (Holder)))
                          and then not Tried (Deal, Office, Holder)
                        then
                           Allocated (Office) := True;
                           Nominated (Holder) := True;
                           Add (Deal, Rec.Faction, Choice);
                           State.Log
                             (Ada.Strings.Unbounded.To_String (Rec.Name)
                              & " suggests "
                              & State.Senator_Name_And_Faction (Holder)
                              & " for "
                              & Office'Image);
                           Current_Try (Office) :=
                             To_Nullable_Id (Holder);

                           Success := True;
                           exit;
                        end if;
                     end;
                  end if;
               end loop;

               if not Success then
                  State.Log
                    (Ada.Strings.Unbounded.To_String (Rec.Name)
                     & " rejects remaining offices");
                  Tried_List.Append (Current_Try);
               end if;

               exit when not Success;
            end loop;

            exit when Is_Empty (Deal);

         end loop;

         exit when Success;

         if not Retry then
            State.Log ("Retrying ....");
         end if;

      end loop;

      if not Success then
         State.Log
           ("could not find a deal");
         Clear (Deal);
         return Deal;
      end if;

      if State.Crisis then
         declare
            Dictator : Senator_Id := Senator_Id'First;
            Highest  : Attribute_Range := Attribute_Range'First;
            Wars     : constant War_Id_Array := State.Active_Wars;
            First    : Boolean := False;
         begin
            for Sid of Agrippa.Cards.Senators.All_Senators loop
               if State.Has_Faction (Sid)
                 and then not Nominated (Sid)
               then
                  declare
                     This_Score : constant Attribute_Range :=
                                    State.Military (Sid);
                  begin
                     if First or else This_Score > Highest then
                        Highest  := This_Score;
                        Dictator := Sid;
                     elsif Highest > Attribute_Range'First
                       and then This_Score = Highest
                     then
                        declare
                           use Agrippa.State;
                           Current  : Senator_State_Interface'Class
                           renames State.Get_Senator_State (Dictator);
                           This     : Senator_State_Interface'Class
                           renames State.Get_Senator_State (Sid);
                        begin
                           for War of Wars loop
                              if Current.Voids_DS (War)
                                and then not This.Voids_DS (War)
                              then
                                 exit;
                              elsif This.Voids_DS (War)
                                and then not Current.Voids_DS (War)
                              then
                                 Dictator := Sid;
                                 exit;
                              end if;
                           end loop;
                        end;
                     end if;
                  end;
                  First := False;
               end if;
            end loop;

            if not First then
               Add (Deal, State.Senator_Faction (Dictator),
                    Office (Agrippa.Dictator, Dictator));
            end if;
         end;
      end if;

      if not Success then
         State.Log
           ("failed to find a deal");
         Clear (Deal);
      end if;

      return Deal;
   end Create_Election_Deal;

   ---------------------
   -- Create_Proposal --
   ---------------------

   procedure Create_Proposal
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Category  : Agrippa.Proposals.Proposal_Category_Type;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type)
   is
      use Agrippa.Proposals;
   begin

      case Category is

         when No_Proposal =>
            raise Constraint_Error with "robot: cannot create no proposal";

         when Office_Nomination =>
            raise Constraint_Error with
              "robot: cannot create office proposal";

         when Governor_Nomination =>
            null;

         when Consul_For_Life =>
            null;

         when Recruitment =>
            null;

         when Attack =>
            declare
               Active_Wars     : constant War_Id_Array :=
                                   State.Active_Wars;
               Inactive_Wars   : constant War_Id_Array :=
                                   State.Inactive_Wars;
               Prosecuted_Wars : constant War_Id_Array :=
                                   State.Prosecuted_Wars;
               Attacked       : Boolean := False;
            begin

               State.Log ("prosecuted wars:"
                          & Natural'Image (Prosecuted_Wars'Length));

               State.Log
                 ("active wars:"
                  & Natural'Image (Active_Wars'Length));

               if Active_Wars'Length > 0 then
                  Find_Unprosecuted_War :
                  for Minimal in Boolean loop
                     for War of Active_Wars loop
                        if not State.Has_Commander (War) then
                           Attacked := Attack_War
                             (Robot     => Robot,
                              State     => State,
                              War       => War,
                              Minimal   => Minimal,
                              Overwhelm => False,
                              Coalition => Coalition,
                              Container => Container);
                           exit Find_Unprosecuted_War when Attacked;
                        end if;
                     end loop;
                  end loop Find_Unprosecuted_War;
               elsif Inactive_Wars'Length > 0 then
                  for War of Inactive_Wars loop
                     exit when Attack_War
                       (Robot     => Robot,
                        State     => State,
                        War       => War,
                        Minimal   => False,
                        Overwhelm => False,
                        Coalition => Coalition,
                        Container => Container);
                  end loop;
               else
                  declare
                     Current_Legions : constant Legion_Count :=
                                         State.Total_Legion_Count;
                     Recruit_Legions : constant Legion_Count :=
                                         (if Current_Legions >= 10 then 0
                                          else 10 - Current_Legions);
                     Current_Fleets  : constant Fleet_Count :=
                                         State.Total_Fleet_Count;
                     Recruit_Fleets  : constant Fleet_Count :=
                                         (if Current_Fleets >= 5 then 0
                                          else 5 - Current_Fleets);
                  begin
                     if Recruit_Legions > 0 or else Recruit_Fleets > 0 then
                        Add_Proposal
                          (Container,
                           Agrippa.Proposals.Recruit
                             (Legions => Recruit_Legions,
                              Fleets  => Recruit_Fleets));
                     end if;
                  end;
               end if;
            end;
      end case;

   end Create_Proposal;

   ---------------------
   -- Create_Proposal --
   ---------------------

   function Create_Proposal
     (Robot   : in out Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Make_Proposal_Message)
      return Agrippa.Messages.Message_Type
   is
      Coalition_Index : Natural := 1;

      function Make_Deal_Proposals
        (Deal   : Agrippa.Deals.Deal_Type;
         Filter : Office_Array)
         return Agrippa.Proposals.Proposal_Container_Type;

      -------------------------
      -- Make_Deal_Proposals --
      -------------------------

      function Make_Deal_Proposals
        (Deal   : Agrippa.Deals.Deal_Type;
         Filter : Office_Array)
         return Agrippa.Proposals.Proposal_Container_Type
      is
         Result : Agrippa.Proposals.Proposal_Container_Type;

         procedure Add_Offer
           (Faction : Faction_Id;
            Offer   : Agrippa.Deals.Offer_Type);

         ---------------
         -- Add_Offer --
         ---------------

         procedure Add_Offer
           (Faction : Faction_Id;
            Offer   : Agrippa.Deals.Offer_Type)
         is
            pragma Unreferenced (Faction);
         begin
            if Agrippa.Deals.Is_Office_Offer (Offer)
              and then (for some Office of Filter =>
                          Agrippa.Deals.Get_Office (Offer) = Office)
            then
               Agrippa.Proposals.Add_Proposal
                 (Result,
                  Agrippa.Proposals.Nominate
                    (Senator => Agrippa.Deals.Get_Holder (Offer),
                     Office  => Agrippa.Deals.Get_Office (Offer)));
            end if;
         end Add_Offer;

      begin
         Agrippa.Deals.Scan (Deal, Add_Offer'Access);
         return Result;
      end Make_Deal_Proposals;

   begin
      loop
         declare
            use Agrippa.Messages;
            Coalition : Coalition_Type :=
                          Robot.Create_Coalition (State, Coalition_Index);
            Proposals : Agrippa.Proposals.Proposal_Container_Type;
            Offers    : array (Faction_Id) of Agrippa.Deals.Offer_List;
            Deal      : Agrippa.Deals.Deal_Type;
         begin

            Coalition_Index := Coalition.Index;

            State.Log
              (State.Faction_Name (Robot.Faction)
               & ": trying coalition" & Coalition_Index'Image & ": "
               & Show (Coalition));

            Coalition.Index := Coalition_Index;

            if Has_Proposal_Office (Message, Rome_Consul) then
               for Faction in Faction_Id loop
                  if Faction /= Robot.Faction then
                     Robot.Handlers (Faction).Senate_Phase_Desire (State);
                     Robot.Handlers (Faction).Get_Offer_Reply
                       (Offers (Faction));
                  else
                     Offers (Faction) := Robot.Senate_Phase_Desire (State);
                  end if;

                  State.Log
                    (State.Faction_Name (Faction)
                     & " wants "
                     & Agrippa.Deals.Show (Offers (Faction), State));
               end loop;

               for Rec of Coalition.Vector loop
                  for Offer of
                    Agrippa.Deals.Get_Offers (Offers (Rec.Faction))
                  loop
                     Rec.Desire.Append (Offer);
                  end loop;
               end loop;

               Deal :=
                 Create_Election_Deal (Robot, State, Coalition.Vector);

               if not Agrippa.Deals.Is_Empty (Deal) then
                  State.Log
                    ("Deal: "
                     & Agrippa.Deals.Show (Deal, State));

                  declare
                     Accepted : Boolean := True;
                  begin
                     for Rec of Coalition.Vector loop
                        declare
                           Agreed : Boolean;
                        begin
                           if Rec.Faction /= Robot.Faction then
                              Robot.Handlers (Rec.Faction).Will_You_Agree_To
                                (State, Deal);
                              Robot.Handlers (Rec.Faction).Get_Agreement_Reply
                                (Agreed);
                              if not Agreed then
                                 State.Log
                                   (State.Faction_Name (Rec.Faction)
                                    & " vetos deal!");
                                 Accepted := False;
                                 exit;
                              end if;
                           end if;
                        end;
                     end loop;

                     if Accepted then
                        Robot.Current_Deal := Deal;
                        return Make_Proposal
                          (Message,
                           Make_Deal_Proposals
                             (Deal, (Rome_Consul, Field_Consul)));
                     end if;
                  end;
               end if;
            elsif Has_Proposal_Category
              (Message, Agrippa.Proposals.Office_Nomination)
            then
               declare
                  Proposal : constant Message_Type :=
                               Make_Proposal
                                 (Message,
                                  Make_Deal_Proposals
                                    (Robot.Current_Deal,
                                     Proposal_Offices (Message)));
               begin
                  if Proposal.Content /= Empty_Message then
                     return Proposal;
                  end if;
               end;

            else

               for Category in Agrippa.Proposals.Proposal_Category_Type loop
                  if Has_Proposal_Category (Message, Category) then
                     Robot.Create_Proposal
                       (State, Category, Coalition.Vector, Proposals);
                  end if;
               end loop;
               return Make_Proposal (Message, Proposals);
            end if;
         end;

         exit when Coalition_Index = 0;

      end loop;

      return Agrippa.Messages.Empty_Message;

   end Create_Proposal;

   -------------------------
   -- Create_Robot_Player --
   -------------------------

   function Create_Robot_Player
     (State        : in out Agrippa.State.State_Interface'Class;
      Faction_Type : Robot_Faction_Type;
      Faction      : Faction_Id)
      return Autoplayer_Interface'Class
   is
      Robot : Robot_Player_Type;
   begin
      Robot.Robot_Faction := Faction_Type;
      Robot.Faction := Faction;
      Robot.Handler :=
        Agrippa.Players.Autohandler.Create_Autohandler
          (State, Faction);
      return Robot;
   end Create_Robot_Player;

   -----------------------
   -- Desired_Candidate --
   -----------------------

   function Desired_Candidate
     (Robot  : Robot_Player_Type'Class;
      State  : Agrippa.State.State_Interface'Class;
      Office : Office_Type)
      return Nullable_Senator_Id
   is
      function Score_Candidate
        (Senator : Senator_Id)
         return Natural;

      ---------------------
      -- Score_Candidate --
      ---------------------

      function Score_Candidate
        (Senator : Senator_Id)
         return Natural
      is
         Base : constant Natural := 50;

         Pop  : constant Popularity_Range := State.Popularity (Senator);
         Inf  : constant Influence_Range := State.Influence (Senator);
         Mil  : constant Attribute_Range := State.Military (Senator);

         Mine : constant Boolean :=
                  State.Senator_Faction (Senator) = Robot.Faction;
         Fac_Bonus : Integer := 50;
         Pop_Bonus : Integer := 0;
         Inf_Bonus : Integer := 0;
         Mil_Bonus : Integer := 0;
      begin
         if Office = Rome_Consul then
            if Integer (Pop) < Natural (State.Current_Unrest) then
               Pop_Bonus := -45;
            else
               Pop_Bonus := 2 * Natural (State.Current_Unrest)
                 + Integer (Pop);
            end if;
         elsif Office = Dictator or else Office = Master_Of_Horse then
            Mil_Bonus := Natural (Mil);
            Fac_Bonus := 1;
         elsif Office = Censor then
            Inf_Bonus := 10 - Integer (Inf);
         elsif Office = Field_Consul then
            Mil_Bonus := Natural (Mil);
            Inf_Bonus :=
              (if Mine
               then 10 - Natural (Inf)
               else 3 - Natural (Inf));

            declare
               Active_Wars   : constant War_Id_Array := State.Active_Wars;
               Inactive_Wars : constant War_Id_Array := State.Inactive_Wars;
            begin
               if Active_Wars'Length = 0 then
                  if Inactive_Wars'Length > 0 then
                     Fac_Bonus := 5;
                  end if;

                  for War of Inactive_Wars loop
                     if State.Get_Senator_State (Senator).Voids_DS (War) then
                        Mil_Bonus := Mil_Bonus * 3;
                        exit;
                     end if;
                  end loop;
               else
                  Fac_Bonus := 3;
                  for War of Active_Wars loop
                     if State.Get_Senator_State (Senator).Voids_DS (War) then
                        Fac_Bonus := 1;
                        Mil_Bonus := Mil_Bonus * 5;
                        exit;
                     end if;
                  end loop;
               end if;
            end;
         end if;

         return Integer'Max
           (Base + (if Mine then Fac_Bonus else 0)
            + Pop_Bonus + Inf_Bonus + Mil_Bonus, 0);
      end Score_Candidate;

      Highest : Natural := 0;
      Result  : Nullable_Senator_Id := No_Senator;
      Sids    : constant Senator_Id_Array :=
                  State.Qualifying_Senators (Office);
   begin
      if False then
         State.Log (State.Faction_Name (Robot.Faction)
                    & " for " & Office'Image);
      end if;

      for Sid of Sids loop
         declare
            Score : constant Natural :=
                      Score_Candidate (Sid);
         begin
            if False then
               State.Log
                 (State.Senator_Name_And_Faction (Sid)
                  & " scores" & Score'Image);
            end if;

            if Score > Highest then
               Result := To_Nullable_Id (Sid);
               Highest := Score;
            end if;
         end;
      end loop;
      return Result;
   end Desired_Candidate;

   --------------------
   -- Desired_Spoils --
   --------------------

   function Desired_Spoils
     (Robot  : Robot_Player_Type'Class;
      State  : Agrippa.State.State_Interface'Class)
      return Agrippa.Deals.Offer_List
   is
      use Agrippa.Deals;
      Result : Offer_List;

      procedure Add (Office : Office_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Office : Office_Type) is
         S : constant Nullable_Senator_Id :=
               Robot.Desired_Candidate (State, Office);
      begin
         if S /= No_Senator then
            Add (Result,
                 Agrippa.Deals.Office
                   (Office, To_Senator_Id (S)));
         end if;
      end Add;
   begin
      case Robot.Robot_Faction is
         when Conservative =>
            Add (Rome_Consul);
            Add (Censor);
            Add (Field_Consul);
            Add (Result, Province);
            Add (Result, Concession);

         when Imperial =>
            Add (Field_Consul);
            Add (Rome_Consul);
            Add (Result, Province);
            Add (Censor);
            Add (Result, Concession);

         when Plutocratic =>
            Add (Censor);
            Add (Result, Concession);
            Add (Result, Province);
            Add (Rome_Consul);
            Add (Field_Consul);

         when Populist =>
            Add (Rome_Consul);
            Add (Field_Consul);
            Add (Censor);
            Add (Result, Concession);
            Add (Result, Province);

      end case;

      return Result;

   end Desired_Spoils;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Robot   : in out Robot_Player_Type;
      State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
   is
      pragma Unreferenced (State);
   begin
      Robot.Faction := Faction;
   end Initialize;

   -----------------------------
   -- Make_Persuasion_Attempt --
   -----------------------------

   function Make_Persuasion_Attempt
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Persuader : out Senator_Id;
      Target    : out Senator_Id;
      Spend     : out Talents)
      return Boolean
   is
      Senators      : constant Senator_Id_Array :=
                        State.Faction_Senators (Robot.Faction);
      Highest_Score : Natural := 0;

      Curia : constant Senator_Id_Array :=
                State.Curia_Senators;
   begin
      for Senator of Senators loop
         declare
            This_Score : constant Natural :=
                           Natural (State.Oratory (Senator))
                           + Natural (State.Influence (Senator));
         begin
            if This_Score > Highest_Score then
               Highest_Score := This_Score;
               Persuader := Senator;
            end if;
         end;
      end loop;

      if Highest_Score = 0 then
         return False;
      end if;

      Highest_Score := 0;

      for Senator of Curia loop
         declare
            This_Score : constant Natural :=
                           (1000 - Natural (State.Loyalty (Senator))
                            - Natural (State.Senator_Treasury (Senator)))
                           * 20 + Natural (State.Military (Senator))
                           + Natural (State.Oratory (Senator));
         begin
            if This_Score > Highest_Score then
               Highest_Score := This_Score;
               Target := Senator;
            end if;
         end;
      end loop;

      if Highest_Score = 0 then
         return False;
      end if;

      declare
         Base : constant Integer :=
                  Natural (State.Oratory (Persuader))
                  + Natural (State.Influence (Persuader))
                  - Natural (State.Loyalty (Target))
                  - Natural (State.Senator_Treasury (Target));
      begin
         if Base < 7 and then
           Natural (State.Senator_Treasury (Persuader)) + Base >= 7
         then
            Spend := Talents (7 - Base);
         else
            Spend := 0;
         end if;
         if Spend = 0 and then Base < 2 then
            return False;
         end if;
      end;

      return True;

   end Make_Persuasion_Attempt;

   ---------------
   -- Play_Card --
   ---------------

   procedure Play_Card
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Senator : out Senator_Id;
      Card    : out Card_Id)
   is
      Available : constant Card_Id_Array :=
                    State.Faction_Cards (Robot.Faction);
   begin
      Card := No_Card;
      Senator := 1;
      for Id of Available loop
         declare
            use all type Agrippa.Cards.Card_Class;
            Card_Rec : constant Agrippa.Cards.Card_Type'Class :=
                         Agrippa.Cards.Card (Id);
         begin
            case Card_Rec.Class is
               when Concession_Card =>
                  Card := Card_Rec.Id;
                  declare
                     Shortest : Natural := Natural'Last;
                  begin
                     for Sid of State.Faction_Senators (Robot.Faction) loop
                        declare
                           Current : constant Concession_Id_Array :=
                                       State.Get_Senator_State (Sid)
                                       .Concessions;
                        begin
                           if Current'Length < Shortest then
                              Shortest := Current'Length;
                              Senator  := Sid;
                           end if;
                        end;
                     end loop;
                  end;
                  exit;

               when Intrigue_Card =>
                  null;
               when Senator_Card =>
                  null;
               when Statesman_Card =>
                  declare
                     use Agrippa.State;
                     use Agrippa.Cards.Statesmen;
                     Statesman : constant Statesman_Card_Type :=
                                   Statesman_Card_Type
                                     (Card_Rec);
                     Family    : constant Senator_Id :=
                                   Statesman.Family;
                     Senator   : constant Senator_State_Interface'Class :=
                                   State.Get_Senator_State (Family);
                  begin
                     if not Senator.Has_Faction
                       or else Senator.Faction = Robot.Faction
                     then
                        Card := Card_Rec.Id;
                        exit;
                     end if;
                  end;

               when others =>
                  raise Constraint_Error with
                  State.Faction_Name (Robot.Faction)
                    & "unexpected card " & Card_Rec.Tag & " in hand";
            end case;
         end;
      end loop;
   end Play_Card;

   ---------------------------
   -- Proposal_Matches_Deal --
   ---------------------------

   function Proposal_Matches_Deal
     (Faction  : Faction_Id;
      State    : Agrippa.State.State_Interface'Class;
      Deal     : Agrippa.Deals.Deal_Type;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
      return Boolean
   is
      pragma Unreferenced (Faction);

      Arr      : constant Agrippa.Proposals.Proposal_Array :=
                   Agrippa.Proposals.Get_Proposals (Proposal);

      function Check_Proposal
        (P : Agrippa.Proposals.Proposal_Type)
         return Boolean;

      --------------------
      -- Check_Proposal --
      --------------------

      function Check_Proposal
        (P : Agrippa.Proposals.Proposal_Type)
         return Boolean
      is
         use all type Agrippa.Proposals.Proposal_Category_Type;
      begin
         case P.Category is
            when No_Proposal =>
               return True;
            when Office_Nomination =>

               if Agrippa.Deals.Contradicts
                 (Deal, Agrippa.Proposals.Office (P),
                  Agrippa.Proposals.Nominee (P),
                  State.Senator_Faction
                    (Agrippa.Proposals.Nominee (P)))
               then
                  return False;
               end if;
               return True;

            when Governor_Nomination =>
               return True;
            when Consul_For_Life =>
               return False;
            when Recruitment =>
               return True;
            when Attack =>
               return True;
         end case;
      end Check_Proposal;

   begin
      for Proposal of Arr loop
         if not Check_Proposal (Proposal) then
            return False;
         end if;
      end loop;
      return True;

   end Proposal_Matches_Deal;

   ---------------------
   -- Score_Proposals --
   ---------------------

   function Score_Proposals
     (Faction  : Faction_Id;
      State    : Agrippa.State.State_Interface'Class;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
      return Integer
   is
      Arr      : constant Agrippa.Proposals.Proposal_Array :=
                   Agrippa.Proposals.Get_Proposals (Proposal);

      Score : Integer := 0;

      function Score_Proposal
        (P : Agrippa.Proposals.Proposal_Type)
         return Integer;

      --------------------
      -- Score_Proposal --
      --------------------

      function Score_Proposal
        (P : Agrippa.Proposals.Proposal_Type)
         return Integer
      is
         use all type Agrippa.Proposals.Proposal_Category_Type;
      begin
         case P.Category is
            when No_Proposal =>
               return 0;
            when Office_Nomination =>
               declare
                  Senator : constant Senator_Id :=
                              Agrippa.Proposals.Nominee (P);
                  Office  : constant Office_Type :=
                              Agrippa.Proposals.Office (P);
               begin
                  if State.Senator_Faction (Senator) = Faction then
                     return 2 * Office_Type'Pos (Office) + 1;
                  else
                     return 0;
                  end if;
               end;

            when Governor_Nomination =>
               return 0;
            when Consul_For_Life =>
               return 0;
            when Recruitment =>
               return 0;
            when Attack =>
               return 0;
         end case;
      end Score_Proposal;

   begin

      for Proposal of Arr loop
         Score := Score + Score_Proposal (Proposal);
      end loop;

      return Score;
   end Score_Proposals;

   -------------------------
   -- Senate_Phase_Desire --
   -------------------------

   overriding function Senate_Phase_Desire
     (Robot   : Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class)
      return Agrippa.Deals.Offer_List
   is
      use Agrippa.Deals;

      Desires : Offer_List;

      procedure Add_Office_Desire
        (Offer : Offer_Type);

      -----------------------
      -- Add_Office_Desire --
      -----------------------

      procedure Add_Office_Desire
        (Offer : Offer_Type)
      is
      begin
         if Offer.Category = Office then
            Add (Desires, Offer);
         end if;
      end Add_Office_Desire;

   begin
      Scan (Robot_Player_Type'Class (Robot).Desired_Spoils (State),
            Add_Office_Desire'Access);
      return Desires;
   end Senate_Phase_Desire;

   ------------------
   -- Send_Message --
   ------------------

   overriding function Send_Message
     (Robot   : in out Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
      return Agrippa.Messages.Message_Type
   is
      use Agrippa.Messages;

   begin
      case Message.Content is
         when Attract_Knights =>
            declare
               Senator : Senator_Id;
               Spend   : Talents;
            begin
               Robot.Attract_Knights (State, Senator, Spend);
               return Attract_Knights (Message, Senator, Spend);
            end;
         when Faction_Transfers =>
            declare
               Response : Message_Type := Message;
               Total    : Talents := State.Faction_Treasury (Robot.Faction);
            begin
               for Sid of State.Faction_Senators (Robot.Faction) loop
                  declare
                     This : constant Talents := State.Senator_Treasury (Sid);
                  begin
                     Total := Total + This;

                     if This > 0 then
                        Add_Transfer_From_Senator
                          (Response, Sid, This);
                     end if;
                  end;
               end loop;

               case Robot.Robot_Faction is
                  when Conservative =>
                     Add_Transfer_To_Senator
                       (Response, State.Faction_Leader (Robot.Faction),
                        (Total + 1) / 2);

                  when Imperial =>
                     declare
                        Roll : constant Die_Range :=
                                 Agrippa.Dice.Roll_Die;
                        Leader_Cash : constant Talents :=
                                        Talents'Min (Talents (Roll),
                                                     Total);
                     begin
                        Add_Transfer_To_Senator
                          (Response, State.Faction_Leader (Robot.Faction),
                           Leader_Cash);
                     end;

                  when Plutocratic =>
                     declare
                        Sids : constant Senator_Id_Array :=
                                 State.Faction_Senators (Robot.Faction);
                     begin
                        for Sid of Sids loop
                           Add_Transfer_To_Senator
                             (Response, Sid, Total / Sids'Length);
                        end loop;
                     end;

                  when Populist =>
                     declare
                        Remaining : Talents := Total;
                        Sids      : constant Senator_Id_Array :=
                                      State.Faction_Senators (Robot.Faction);
                     begin
                        for Sid of Sids loop
                           exit when Remaining = 0;
                           if not State.Is_Faction_Leader (Sid) then
                              Add_Transfer_To_Senator
                                (Response, Sid, 1);
                              Remaining := Remaining - 1;
                           end if;
                        end loop;

                        if Remaining > 1 then
                           Add_Transfer_To_Senator
                             (Response, State.Faction_Leader (Robot.Faction),
                              Remaining - 1);
                        end if;
                     end;
               end case;

               return Response;
            end;

         when Persuasion_Attempt =>

            declare
               Persuader, Target : Senator_Id;
               Bribe             : Talents;
            begin
               if Make_Persuasion_Attempt
                 (Robot, State, Persuader, Target, Bribe)
               then
                  return Persuasion_Attempt
                    (Message, Persuader, Target, Bribe);
               else
                  return Empty_Message;
               end if;
            end;

         when Make_Proposal =>

            return Robot.Create_Proposal (State, Message);

         when Player_Action =>

            if Allowed_Action (Message, Check_Rebellion) then
               return Check_Rebellion_Action (Message, False);
            elsif Allowed_Action (Message, Play_Card) then
               declare
                  Card : Card_Id;
                  Senator : Senator_Id;
               begin
                  Robot.Play_Card (State, Senator, Card);
                  if Card /= No_Card then
                     return Play_Card_Action
                       (Message, Senator, Card);
                  else
                     return Empty_Message;
                  end if;
               end;
            else
               return Empty_Message;
            end if;

         when others =>
            raise Constraint_Error with
              "player cannot accept message: " & Message.Content'Image;
      end case;
   end Send_Message;

   -----------------
   -- Set_Players --
   -----------------

   overriding procedure Set_Players
     (Robot   : in out Robot_Player_Type;
      Players : Player_Access_Array)
   is
   begin
      Robot.Handlers := Players;
      Robot.Handler := Players (Robot.Faction);
      Agrippa.Players.Autohandler.Set_Autoplayer
        (Players (Robot.Faction), Robot);
   end Set_Players;

   ----------
   -- Show --
   ----------

   function Show (Coalition : Coalition_Type) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Rec of Coalition.Vector loop
         if Result /= "" then
            Result := Result & ", ";
         end if;
         Result := Result & Rec.Name;
      end loop;
      return To_String (Result);
   end Show;

   ----------------
   -- Start_Turn --
   ----------------

   overriding procedure Start_Turn
     (Robot   : Robot_Player_Type;
      State   : in out Agrippa.State.State_Interface'Class)
   is
      Sids       : constant Senator_Id_Array :=
                     State.Faction_Senators (Robot.Faction);
      Best       : Senator_Id;
      Best_Score : Integer := Integer'First;

      function Score (Id : Senator_Id) return Integer;

      -----------
      -- Score --
      -----------

      function Score (Id : Senator_Id) return Integer is
      begin
         case Robot.Robot_Faction is
            when Conservative =>
               return Integer (State.Influence (Id));
            when Imperial =>
               return Integer (State.Military (Id));
            when Plutocratic =>
               return Integer (State.Influence (Id));
            when Populist =>
               return Integer (State.Influence (Id))
                 + Integer (State.Popularity (Id));
         end case;
      end Score;

   begin
      for Id of Sids loop
         declare
            This_Score : constant Integer := Score (Id);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best := Id;
            end if;
         end;
      end loop;

      if not State.Has_Faction_Leader (Robot.Faction)
        or else State.Faction_Leader (Robot.Faction) /= Best
      then
         State.Set_Faction_Leader (Robot.Faction, Best);
      end if;
   end Start_Turn;

   ----------
   -- Vote --
   ----------

   overriding function Vote
     (Player    : Robot_Player_Type;
      State     : Agrippa.State.State_Interface'Class;
      Sponsor   : Senator_Id;
      Proposals : Agrippa.Proposals.Proposal_Container_Type)
      return Faction_Vote_Type
   is
   begin
      return Votes : Faction_Vote_Type := (others => 0) do
         if State.Senator_Faction (Sponsor) = Player.Faction
           or else Proposal_Matches_Deal
             (Player.Faction, State, Player.Current_Deal, Proposals)
           or else Score_Proposals (Player.Faction, State, Proposals) > 0
         then
            Votes (Aye) := State.Faction_Votes (Player.Faction);
         else
            Votes (Nay) := State.Faction_Votes (Player.Faction);
         end if;
      end return;
   end Vote;

   --------------------------
   -- What_Do_You_Want_For --
   --------------------------

   overriding function What_Do_You_Want_For
     (Robot   : Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Offer   : Agrippa.Deals.Offer_List)
      return Agrippa.Deals.Offer_List
   is
      pragma Unreferenced (Robot, State);

      Response : Agrippa.Deals.Offer_List;

      procedure Evaluate_Request (Request : Agrippa.Deals.Offer_Type);

      ----------------------
      -- Evaluate_Request --
      ----------------------

      procedure Evaluate_Request (Request : Agrippa.Deals.Offer_Type) is null;

   begin

      Agrippa.Deals.Scan (Offer, Evaluate_Request'Access);

      if Agrippa.Deals.Is_Empty (Response) then
         Agrippa.Deals.Add (Response, Agrippa.Deals.Nothing);
      end if;
      return Response;
   end What_Do_You_Want_For;

   ----------------------------
   -- What_Will_You_Give_For --
   ----------------------------

   overriding function What_Will_You_Give_For
     (Robot   : Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Offer   : Agrippa.Deals.Offer_List)
      return Agrippa.Deals.Offer_List
   is
      pragma Unreferenced (Robot, State, Offer);
      Result : Agrippa.Deals.Offer_List;
   begin
      Agrippa.Deals.Clear (Result);
      return Result;
   end What_Will_You_Give_For;

   -----------------------
   -- Will_You_Agree_To --
   -----------------------

   overriding function Will_You_Agree_To
     (Robot   : in out Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Deal    : Agrippa.Deals.Deal_Type)
      return Boolean
   is
      Score : Integer := 0;
      Spoils : constant Agrippa.Deals.Offer_List :=
                 Robot_Player_Type'Class (Robot).Desired_Spoils (State);
      procedure Check_Terms
        (Faction : Faction_Id;
         Terms   : Agrippa.Deals.Offer_List);

      -----------------
      -- Check_Terms --
      -----------------

      procedure Check_Terms
        (Faction : Faction_Id;
         Terms   : Agrippa.Deals.Offer_List)
      is
         pragma Unreferenced (Faction);
         This_Score : Natural := 0;

         procedure Check_My_Offer (Offer : Agrippa.Deals.Offer_Type);

         --------------------
         -- Check_My_Offer --
         --------------------

         procedure Check_My_Offer (Offer : Agrippa.Deals.Offer_Type) is
            use Agrippa.Deals;
            Index : constant Natural :=
                      Matching_Index (Spoils, Offer);
         begin
            This_Score :=
              This_Score
                + (case Index is
                      when 1 | 2 => 8,
                      when 3 | 4 => 4,
                      when others => 2);
         end Check_My_Offer;

      begin
         Agrippa.Deals.Scan (Terms, Check_My_Offer'Access);
         Score := Score + This_Score;
      end Check_Terms;

   begin
      Agrippa.Deals.Scan (Deal, Check_Terms'Access);
      State.Log
        (State.Faction_Name (Robot.Faction)
         & " scores deal" & Score'Image);
      if Score > 0 then
         State.Log
           (State.Faction_Name (Robot.Faction)
            & " agrees");
         Robot.Current_Deal := Deal;
         return True;
      else
         State.Log
           (State.Faction_Name (Robot.Faction)
            & " rejects");
         return False;
      end if;
   end Will_You_Agree_To;

end Agrippa.Players.Robots;
