with Ada.Containers.Vectors;
with Ada.Text_IO;

with Agrippa.Dice;
with Agrippa.Proposals;

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

   type Faction_Record is
      record
         Faction   : Faction_Id;
         Votes     : Vote_Count;
         Influence : Faction_Influence_Range;
         Senators  : Senator_Vectors.Vector;
      end record;

   function Create
     (State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
      return Faction_Record;

   function Highest_Score
     (Faction : Faction_Record;
      Score   : not null access
        function (Senator : Senator_Record) return Integer)
     return Senator_Id;

   package Faction_Vectors is
     new Ada.Containers.Vectors (Positive, Faction_Record);

   function More_Votes (Left, Right : Faction_Record) return Boolean is
     (Left.Votes > Right.Votes
      or else (Left.Votes = Right.Votes
               and then Left.Influence > Right.Influence));

   package Faction_Sorting is
     new Faction_Vectors.Generic_Sorting (More_Votes);

   type Robot_Player_Type is
     new Player_Interface with
      record
         Robot_Faction : Robot_Faction_Type;
         Faction       : Faction_Id;
      end record;

   overriding function Name
     (Player : Robot_Player_Type)
      return String
   is (case Player.Robot_Faction is
          when Conservative => "Conservative Faction",
          when Imperial     => "Imperial Faction",
          when Plutocratic  => "Plutocratic Faction",
          when Populist     => "Populist Faction");

   overriding procedure Initialize
     (Robot   : in out Robot_Player_Type;
      State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id);

   overriding procedure Start_Turn
     (Robot   : in out Robot_Player_Type;
      State   : in out Agrippa.State.State_Interface'Class);

   overriding function Send_Message
     (Robot   : Robot_Player_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
      return Agrippa.Messages.Message_Type;

   procedure Attract_Knights
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class;
      Senator : out Senator_Id;
      Spend   : out Talents);

   function Create_Coalition
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class)
      return Faction_Vectors.Vector;

   procedure Create_Proposal
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      Category  : Agrippa.Proposals.Proposal_Category_Type;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type);

   procedure Attack_War
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      War       : War_Id;
      Overwhelm : Boolean;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type);

   ----------------
   -- Attack_War --
   ----------------

   procedure Attack_War
     (Robot     : Robot_Player_Type'Class;
      State     : Agrippa.State.State_Interface'Class;
      War       : War_Id;
      Overwhelm : Boolean;
      Coalition : Faction_Vectors.Vector;
      Container : in out Agrippa.Proposals.Proposal_Container_Type)
   is
      pragma Unreferenced (Robot, Coalition);
      War_State          : constant Agrippa.State.War_State_Interface'Class :=
                             State.Get_War_State (War);
      Fleet_Strength     : constant Fleet_Count := War_State.Fleet_Strength;
      Fleet_Support      : constant Fleet_Count := War_State.Fleet_Support;
      Required_Fleets    : constant Fleet_Count :=
                             Fleet_Count'Max
                               (Fleet_Count'Max
                                  (Fleet_Strength, Fleet_Support), 5);
      Land_Strength      : constant Legion_Count := War_State.Land_Strength;
      Required_Strength  : constant Legion_Count :=
                             Legion_Count'Max
                               ((if Overwhelm
                                then Land_Strength + 5 else Land_Strength),
                                10);
      Veteran_Legions    : constant Legion_Index_Array :=
                             State.Available_Veteran_Legions;
      Available_Strength : constant Legion_Count :=
                             State.Available_Regular_Legions
                               + 2 * Veteran_Legions'Length;
      Recruit_Legions    : constant Legion_Count :=
                             (if Available_Strength >= Required_Strength
                              then 0
                              else Required_Strength - Available_Strength);
      Available_Fleets   : constant Fleet_Count :=
                             State.Available_Fleets;
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
      Final_Strength        : constant Legion_Count :=
                                Available_Strength
                                  + Final_Recruit_Legions;
      Commander          : constant Senator_Id :=
                             (if State.Has_Holder (Field_Consul)
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

      if Canceled_Recuitment > 0 then
         return;
      end if;

      if Final_Strength < Required_Strength then
         return;
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

      Agrippa.Proposals.Add_Proposal
        (Container => Container,
         Proposal  =>
           Agrippa.Proposals.Attack
             (War             => War,
              Commander       => Commander,
              Regular_Legions =>
                State.Available_Regular_Legions + Final_Recruit_Legions,
              Veteran_Legions => Veteran_Legions,
              Fleets          => Required_Fleets));

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
         Votes     => State.Faction_Votes (Faction),
         Influence => State.Faction_Influence (Faction),
         Senators  => <>)
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
     (Robot   : Robot_Player_Type'Class;
      State   : Agrippa.State.State_Interface'Class)
      return Faction_Vectors.Vector
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
              (3, 4, 5),
              (2, 3, 4),
              (1, 5, 0),
              (1, 4, 0),
              (1, 3, 0),
              (1, 2, 0),
              (1, 2, 5),
              (1, 2, 4),
              (1, 2, 3));

      begin
         for Trial_Index in Coalitions'Range (1) loop
            declare
               X : constant Natural := Coalitions (Trial_Index, 1);
               Y : constant Natural := Coalitions (Trial_Index, 2);
               Z : constant Natural := Coalitions (Trial_Index, 3);
            begin
               if Check_Coalition (X, Y, Z) then
                  return Result : Faction_Vectors.Vector do
                     Result.Append (This_Faction);

                     declare
                        procedure Add (Index : Natural);

                        ---------
                        -- Add --
                        ---------

                        procedure Add (Index : Natural) is
                        begin
                           if Index > 0 then
                              Result.Append (Other_Factions.Element (Index));
                           end if;
                        end Add;

                     begin
                        Add (X);
                        Add (Y);
                        Add (Z);
                     end;

                     Faction_Sorting.Sort (Result);
                  end return;
               end if;
            end;
         end loop;
      end;

      return Result : Faction_Vectors.Vector do
         Result.Append (This_Faction);
         Result.Append (Other_Factions.Element (2));
         Result.Append (Other_Factions.Element (3));
         Result.Append (Other_Factions.Element (4));
         Result.Append (Other_Factions.Element (5));
      end return;
   end Create_Coalition;

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

         when Consular_Nomination =>
            declare
               function Score_Consul_1
                 (Senator : Senator_Record)
                  return Integer
               is (100 - Natural (Senator.Influence));

               Consul_1 : constant Senator_Id :=
                            Highest_Score
                              (Coalition.First_Element, Score_Consul_1'Access);

               function Score_Consul_2
                 (Senator : Senator_Record)
                  return Integer
               is (if Senator.Senator = Consul_1
                   then -100
                   else Natural (Senator.Military));

               Consul_2 : constant Senator_Id :=
                            Highest_Score
                              (Faction => (if Coalition.Last_Index = 1
                                           then Coalition.First_Element
                                           else Coalition.Element (2)),
                               Score   => Score_Consul_2'Access);
            begin
               Add_Proposal
                 (Container, Nominate (Consul_1, Rome_Consul));
               Add_Proposal
                 (Container, Nominate (Consul_2, Field_Consul));
            end;

         when Censor_Nomination =>

            declare
               function Score_Censor
                 (Senator : Senator_Record)
                  return Integer
               is (if State.Has_Office (Senator.Senator)
                   then -1000
                   else 100 - Natural (Senator.Influence));

               Faction_Index : constant Positive :=
                                 Positive'Min (3, Coalition.Last_Index);

               Candidate : constant Senator_Id :=
                             Highest_Score
                                   (Coalition.Element (Faction_Index),
                                    Score_Censor'Access);
            begin
               Add_Proposal
                 (Container, Nominate (Candidate, Censor));
            end;

         when Pontifex_Maximus_Nomination =>
            null;

         when Dictator_Nomination =>
            null;

         when Governor_Nomination =>
            null;

         when Consul_For_Life =>
            null;

         when Recruitment =>
            null;

         when Attack =>
            declare
               Unprosecuted_Wars : constant War_Id_Array :=
                                     State.Unprosecuted_Wars;
               Prosecuted_Wars   : constant War_Id_Array :=
                                     State.Prosecuted_Wars;
               Inactive_Wars     : constant War_Id_Array :=
                                     State.Inactive_Wars;
            begin

               if Unprosecuted_Wars'Length > 0 then
                  Attack_War
                    (Robot     => Robot,
                     State     => State,
                     War       => Unprosecuted_Wars (Unprosecuted_Wars'First),
                     Overwhelm => False,
                     Coalition => Coalition,
                     Container => Container);
               elsif Inactive_Wars'Length > 0
                 and then Prosecuted_Wars'Length = 0
               then
                  Attack_War
                    (Robot     => Robot,
                     State     => State,
                     War       => Inactive_Wars (Inactive_Wars'First),
                     Overwhelm => False,
                     Coalition => Coalition,
                     Container => Container);
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

   -------------------
   -- Highest_Score --
   -------------------

   function Highest_Score
     (Faction : Faction_Record;
      Score   : not null access
        function (Senator : Senator_Record) return Integer)
      return Senator_Id
   is
      Highest : Integer := Integer'First;
      Id      : Nullable_Senator_Id := No_Senator;
   begin
      for Rec of Faction.Senators loop
         if Score (Rec) > Highest then
            Highest := Score (Rec);
            Id     := To_Nullable_Id (Rec.Senator);
         end if;
      end loop;
      pragma Assert (Has_Senator (Id));
      return To_Senator_Id (Id);
   end Highest_Score;

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

   ------------------
   -- Robot_Player --
   ------------------

   function Robot_Player
     (Faction_Type : Robot_Faction_Type)
      return Player_Interface'Class
   is
      Robot : Robot_Player_Type;
   begin
      Robot.Robot_Faction := Faction_Type;
      return Robot;
   end Robot_Player;

   ------------------
   -- Send_Message --
   ------------------

   overriding function Send_Message
     (Robot   : Robot_Player_Type;
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
                        Roll : constant Agrippa.Dice.Die_Range :=
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

         when Make_Proposal =>
            declare
               Coalition : constant Faction_Vectors.Vector :=
                             Robot.Create_Coalition (State);
               Proposals : Agrippa.Proposals.Proposal_Container_Type;
            begin
               Ada.Text_IO.Put ("coalition:");
               for Rec of Coalition loop
                  Ada.Text_IO.Put
                    (" " & State.Faction_Name (Rec.Faction));
               end loop;

               Ada.Text_IO.New_Line;

               for Category in Agrippa.Proposals.Proposal_Category_Type loop
                  if Has_Proposal_Category (Message, Category) then
                     Robot.Create_Proposal
                       (State, Category, Coalition, Proposals);
                  end if;
               end loop;
               return Make_Proposal (Message, Proposals);
            end;

         when others =>
            raise Constraint_Error with
              "player cannot accept message: " & Message.Content'Image;
      end case;
   end Send_Message;

   ----------------
   -- Start_Turn --
   ----------------

   overriding procedure Start_Turn
     (Robot   : in out Robot_Player_Type;
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

end Agrippa.Players.Robots;
