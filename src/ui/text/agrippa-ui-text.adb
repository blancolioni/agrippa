with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Ada.Strings.Unbounded;

with WL.Localisation;
with WL.Numerics.Roman;

with Agrippa.Events;
with Agrippa.Images;
with Agrippa.Scenarios;

with Agrippa.Messages;
with Agrippa.Proposals;

with Agrippa.Cards.Concessions;
with Agrippa.Cards.Wars;

with Agrippa.State.Senators;

with Agrippa.Factions;

with Agrippa.Phases.Sequence;

with Agrippa.Players.Robots.Configure;

package body Agrippa.UI.Text is

   type Text_Notifier_Type is
     new Agrippa.State.Notifications.Change_Handler_Interface with
       null record;

   overriding procedure On_Faction_Leader_Changed
     (Handler : Text_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id);

   overriding procedure Send_Message
     (Handler : Text_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type);

   overriding procedure Send_Notification
     (Handler : Text_Notifier_Type;
      Text    : String);

   overriding procedure On_Faction_Revenue
     (Handler   : Text_Notifier_Type;
      State     : Agrippa.State.State_Interface'Class;
      Faction   : Faction_Id;
      Old_Value : Talents;
      Income    : Talents;
      Provinces : Talents;
      New_Value : Talents);

   overriding procedure On_State_Revenue
     (Handler   : Text_Notifier_Type;
      Items     : Agrippa.State.State_Revenue_Array);

   Text_Notifier : aliased Text_Notifier_Type;

   procedure Start_Turn
     (UI : in out Text_UI_Type'Class);

   procedure State_Of_The_Republic
     (UI : in out Text_UI_Type'Class);

   procedure Put_Heading (Heading : String);

   procedure Put_Field
     (State         : Agrippa.State.State_Interface'Class;
      Field_Name    : String;
      Field_Value   : String;
      Field_Comment : String := "");

   procedure Put_Field (State        : Agrippa.State.State_Interface'Class;
      Field_Name   : String;
      Field_Value  : Integer;
                        Field_Suffix : String := "");

   procedure Put_Faction_Name (Faction : Agrippa.Factions.Faction_Type'Class);

   procedure Put_Faction
     (Game    : Agrippa.Game.Game_Type'Class;
      Faction : Agrippa.Factions.Faction_Type'Class);

   procedure Put_Senators
     (Game    : Agrippa.Game.Game_Type'Class;
      Ids     : Senator_Id_Array);

   procedure Put_Phase
     (Game  : Agrippa.Game.Game_Type'Class;
      Id    : Agrippa.Phases.Sequence.Phase_Id;
      Phase : Agrippa.Phases.Phase_Interface'Class);

   -----------------------
   -- Get_Text_Notifier --
   -----------------------

   function Get_Text_Notifier
     return Agrippa.State.Notifications.Change_Handler_Interface'Class
   is
   begin
      return Notifier : Text_Notifier_Type;
   end Get_Text_Notifier;

   -------------------------------
   -- On_Faction_Leader_Changed --
   -------------------------------

   overriding procedure On_Faction_Leader_Changed
     (Handler : Text_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
   is
      pragma Unreferenced (Handler);
   begin
      if State.Has_Faction_Leader (Faction) then
         Ada.Text_IO.Put_Line
           (State.Senator_Name
              (State.Faction_Leader (Faction))
            & " now leads " & State.Faction_Name (Faction));
      end if;
   end On_Faction_Leader_Changed;

   ------------------------
   -- On_Faction_Revenue --
   ------------------------

   overriding procedure On_Faction_Revenue
     (Handler   : Text_Notifier_Type;
      State     : Agrippa.State.State_Interface'Class;
      Faction   : Faction_Id;
      Old_Value : Talents;
      Income    : Talents;
      Provinces : Talents;
      New_Value : Talents)
   is
      pragma Unreferenced (Handler);
      use Ada.Integer_Text_IO;
      use Ada.Text_IO;
   begin
      if Faction = Faction_Id'First then
         Put ("Faction");
         Set_Col (16);
         Put ("Old  + Income + Provinces = Total");
         New_Line;
      end if;

      Put (State.Faction_Name (Faction));
      Set_Col (16);
      Put (Integer (Old_Value), 3);
      Put ("  +   ");
      Put (Integer (Income), 3);
      Put ("  +   ");
      Put (Integer (Provinces), 3);
      Put ("     = ");
      Put (Integer (New_Value), 3);
      New_Line;
   end On_Faction_Revenue;

   ----------------------
   -- On_State_Revenue --
   ----------------------

   overriding procedure On_State_Revenue
     (Handler   : Text_Notifier_Type;
      Items     : Agrippa.State.State_Revenue_Array)
   is
      pragma Unreferenced (Handler);
      use Agrippa.State;
      use Ada.Text_IO, Ada.Integer_Text_IO;
   begin
      for Item of Items loop
         Put (Tag (Item));
         Set_Col (20);
         Put (Value (Item), 3);
         if Comment (Item) /= "" then
            Put (" (");
            Put (Comment (Item));
            Put (")");
         end if;
         New_Line;
      end loop;
   end On_State_Revenue;

   -----------------
   -- Put_Faction --
   -----------------

   procedure Put_Faction
     (Game    : Agrippa.Game.Game_Type'Class;
      Faction : Agrippa.Factions.Faction_Type'Class)
   is
      use Ada.Text_IO;
   begin
      New_Line;
      Put ("*** ");
      Put_Faction_Name (Faction);
      Set_Col (18);
      Put
        (Game.Local_Text
           ("treasury",
            Agrippa.Images.Image (Faction.Treasury)));
      Put ("   ");
      Put (Game.Local_Text ("influence") & ":");
      Put (Faction_Influence_Range'Image
           (Game.Faction_Influence (Faction.Id)));
      Put ("  ");
      Put (Game.Local_Text ("votes"));
      Put (": ");
      Put (Agrippa.Images.Image (Game.Faction_Votes (Faction.Id)));
      Ada.Text_IO.New_Line;

      Put_Senators
        (Game, Game.Faction_Senators (Faction.Id));
      Put ("     "
           & Agrippa.Images.Image
             (Faction.Card_Count, "card"));
      Ada.Text_IO.New_Line;
   end Put_Faction;

   ----------------------
   -- Put_Faction_Name --
   ----------------------

   procedure Put_Faction_Name
     (Faction : Agrippa.Factions.Faction_Type'Class)
   is
   begin
      Ada.Text_IO.Put (Faction.Name);
   end Put_Faction_Name;

   ---------------
   -- Put_Field --
   ---------------

   procedure Put_Field
     (State         : Agrippa.State.State_Interface'Class;
      Field_Name    : String;
      Field_Value   : String;
      Field_Comment : String := "")
   is
      use Ada.Text_IO;
   begin
      if Field_Name /= "" then
         Put (State.Local_Text (Field_Name));
      end if;
      Set_Col (24);
      Put (Field_Value);
      if Field_Comment /= "" then
         Put (" " & Field_Comment);
      end if;
      New_Line;
   end Put_Field;

   ---------------
   -- Put_Field --
   ---------------

   procedure Put_Field
     (State        : Agrippa.State.State_Interface'Class;
      Field_Name   : String;
      Field_Value  : Integer;
      Field_Suffix : String := "")
   is
   begin
      Put_Field
        (State, Field_Name,
         Agrippa.Images.Trim (Field_Value'Image) & Field_Suffix);
   end Put_Field;

   -----------------
   -- Put_Heading --
   -----------------

   procedure Put_Heading (Heading : String) is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("--- " & Heading & " ---");
   end Put_Heading;

   ---------------
   -- Put_Phase --
   ---------------

   procedure Put_Phase
     (Game  : Agrippa.Game.Game_Type'Class;
      Id    : Agrippa.Phases.Sequence.Phase_Id;
      Phase : Agrippa.Phases.Phase_Interface'Class)
   is
      Name : constant String :=
               Agrippa.Phases.Sequence.Show (Id)
               & ". "
               & Game.Local_Text (Phase.Name);
      Dash : constant String (Name'Range) := (others => '-');
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (Dash);
      Ada.Text_IO.Put_Line (Name);
      Ada.Text_IO.Put_Line (Dash);
   end Put_Phase;

   ------------------
   -- Put_Senators --
   ------------------

   procedure Put_Senators
     (Game : Agrippa.Game.Game_Type'Class;
      Ids : Senator_Id_Array)
   is
      use Ada.Text_IO;
      use Agrippa.Images;
   begin
      Set_Col (18);
      Put_Line (" M  O  L  I  P");

      for Id of Ids loop
         Put ("#");
         Put (Trim (Id'Image));

         declare
            State : constant Agrippa.State.Senators.Senator_State_Type :=
                      Agrippa.State.Senators.Senator_State_Type
                        (Game.Get_Senator_State (Id));

            procedure Put_Attribute (Attribute : Attribute_Range);

            -------------------
            -- Put_Attribute --
            -------------------

            procedure Put_Attribute (Attribute : Attribute_Range) is
            begin
               if Attribute < 10 then
                  Put (" ");
               end if;
               Put (Image (Attribute));
            end Put_Attribute;

         begin
            if State.Has_Statesman then
               Put ("A");
            end if;
            Set_Col (6);
            Put (Game.Senator_Name (Id));
            if Game.Has_Faction (Id)
              and then Game.Faction_Leader (Game.Senator_Faction (Id)) = Id
            then
               Set_Col (16);
               Put ("L");
            end if;

            Set_Col (18);
            Put_Attribute (State.Military);
            Put (" ");
            Put_Attribute (State.Oratory);
            Put (" ");
            Put_Attribute (State.Loyalty);
            Put (" ");
            if State.Influence < 10 then
               Put (" ");
            end if;
            Put (Image (State.Influence));
            Put (" ");
            if State.Popularity /= 0 then
               Put (Popularity_Range'Image (State.Popularity));
               Put (" ");
            else
               Put ("   ");
            end if;

            if State.Prior_Consul then
               Put (" pc");
            end if;

            if State.Has_Office then
               Put (" -");
               case State.Office is
                  when Dictator =>
                     Put ("DI");
                  when Rome_Consul =>
                     Put ("RC");
                  when Field_Consul =>
                     Put ("FC");
                  when Censor =>
                     Put ("CE");
                  when Master_Of_Horse =>
                     Put ("MH");
                  when Pontifex_Maximus =>
                     Put ("PM");
               end case;
               Put ("-");
            end if;

            if State.Knights > 0 then
               Put (" ");
               Put (Agrippa.Images.Image (State.Knights));
               Put ("Kni");
            end if;

            if State.Treasury > 0 then
               Put (" ");
               Put (Agrippa.Images.Image (State.Treasury));
               Put ("t");
            end if;

            declare
               use Agrippa.Cards.Concessions;
            begin
               for Id of State.Concessions loop
                  Put (" " & Game.Local_Text (Concession (Id).Tag));
               end loop;
            end;

         end;

         New_Line;
      end loop;

   end Put_Senators;

   ------------------
   -- Send_Message --
   ------------------

   overriding procedure Send_Message
     (Handler : Text_Notifier_Type;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
   is
      pragma Unreferenced (Handler);
      use Ada.Text_IO;
      use Agrippa.Messages;
   begin
      case Message.Content is
         when Empty_Message =>
            null;
         when Mortality_Roll =>
            Ada.Text_IO.Put
              (State.Local_Text
                 ("roll-of-x",
                  Agrippa.Images.Image (Get_Roll (Message))
                  & ": "));
            if Has_Senator (Message) then
               declare
                  Left    : constant Count := Col;
                  Senator : constant Senator_Id := Get_Senator (Message);
                  Name    : constant String :=
                              State.Senator_Name (Senator);
               begin
                  if State.Has_Faction (Senator) then
                     declare
                        Faction : constant Faction_Id :=
                                    State.Senator_Faction (Senator);
                     begin
                        if State.Has_Faction_Leader (Faction)
                          and then State.Faction_Leader (Faction) = Senator
                        then
                           Ada.Text_IO.Put_Line
                             (State.Local_Text
                                ("faction-leader-of-faction-dies",
                                 Name, State.Faction_Name (Faction)));
                           if not State.Get_Senator_State (Senator)
                             .Is_Statesman_Only
                           then
                              Set_Col (Left);
                              Ada.Text_IO.Put_Line
                                (State.Local_Text
                                   ("succeeded-by-son", Name));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (State.Local_Text
                                ("name-of-faction-dies",
                                 Name, State.Faction_Name (Faction)));
                        end if;
                     end;
                  else
                     Ada.Text_IO.Put_Line
                       (State.Local_Text
                          ("unaligned-dies", Name));
                  end if;
               end;
            elsif Mortality_Roll_Twice (Message) then
               Ada.Text_IO.Put_Line
                 (State.Local_Text ("mortality-roll-twice"));
            elsif Mortality_Roll_None (Message) then
               Ada.Text_IO.Put_Line
                 (State.Local_Text ("mortality-roll-none"));
            else
               raise Constraint_Error with
                 "bad mortality roll message";
            end if;

         when Faction_Transfers =>
            declare

               Faction : constant Faction_Id := Get_Faction (Message);
               Name    : constant String := State.Faction_Name (Faction);

               procedure Report_Faction_Treasury
                 (Take   : Boolean;
                  Amount : Talents);

               procedure Report_Senator_Treasury
                 (Senator : Senator_Id;
                  Take    : Boolean;
                  Amount  : Talents);

               -----------------------------
               -- Report_Faction_Treasury --
               -----------------------------

               procedure Report_Faction_Treasury
                 (Take   : Boolean;
                  Amount : Talents)
               is
               begin
                  if Take then
                     Ada.Text_IO.Put
                       (Name & " transfers "
                        & Agrippa.Images.Image (Amount)
                        & "t");
                  end if;
               end Report_Faction_Treasury;

               -----------------------------
               -- Report_Senator_Treasury --
               -----------------------------

               procedure Report_Senator_Treasury
                 (Senator : Senator_Id;
                  Take    : Boolean;
                  Amount  : Talents)
               is
                  pragma Unreferenced (Amount);
               begin
                  if not Take then
                     Ada.Text_IO.Put_Line
                       (" to " & State.Senator_Name (Senator));
                  end if;
               end Report_Senator_Treasury;

            begin
               Agrippa.Messages.Scan_Transfers
                 (Message,
                  Report_Faction_Treasury'Access,
                  Report_Senator_Treasury'Access);
            end;

         when Initiative_Roll =>

            Ada.Text_IO.Put
              (State.Local_Text
                 ("initiative-roll",
                  Agrippa.Images.Image (Get_Roll (Message))));
            Ada.Text_IO.Put (": ");

            if Event_Initiative_Roll (Message) then
               declare
                  Event_Roll : constant TDR_Range :=
                                 Event_Type (Message);
               begin
                  Ada.Text_IO.Put
                    (State.Local_Text
                       ("event-roll",
                        Agrippa.Images.Image (Event_Roll))
                     & ": " & State.Event_Tag (Event_Roll));
               end;
            else
               declare
                  Card : constant Agrippa.Cards.Card_Type'Class :=
                           Agrippa.Cards.Card
                             (Initiative_Card (Message));
               begin
                  if Card.Keep then
                     Ada.Text_IO.Put
                       (State.Local_Text
                          ("draws-a-red-card"));
                  else
                     Ada.Text_IO.Put
                       (State.Local_Text
                          ("draws-card",
                           State.Local_Text (Card.Tag)));
                  end if;
               end;
            end if;
            Ada.Text_IO.New_Line;

         when Persuasion_Attempt =>
            Ada.Text_IO.Put_Line
              (State.Senator_Name (Get_Senator (Message))
               & " attempts to persuade unaligned "
               & State.Senator_Name (Get_Persuasion_Target (Message))
               & (if Get_Money (Message) = 0 then " with no bribe"
                 else " with bribe of "
                 & Agrippa.Images.Image (Get_Money (Message))
                 & "t"));

         when Attract_Knights =>
            declare
               Senator : constant Senator_Id := Get_Senator (Message);
               Spend   : constant Talents := Get_Money (Message);
               Roll    : constant Positive := Get_Roll (Message);
               Success : constant Boolean := Successful (Message);
            begin
               Ada.Text_IO.Put_Line
                 (State.Senator_Name (Senator)
                  & " spends "
                  & (if Spend = 0 then "no talents"
                    else Agrippa.Images.Image (Spend) & "t")
                  & " to attract a Knight: rolls a "
                  & Agrippa.Images.Image (Roll)
                  & "; "
                  & (if Success then "succeeds!" else "fails!"));
            end;

         when Population_Roll =>

            declare
               HRAO : constant Senator_Id :=
                        State.Highest_Ranking_Available_Officer;
            begin
               Ada.Text_IO.Put_Line
                 (State.Local_Text
                    ("hrao-is-x-of-y",
                     State.Senator_Name (HRAO),
                     State.Faction_Name (State.Senator_Faction (HRAO))));
            end;

            if Has_Table (Message) then
               declare
                  procedure Put (Heading, Value, Comment : String);

                  ---------
                  -- Put --
                  ---------

                  procedure Put (Heading, Value, Comment : String) is
                  begin
                     Put_Field (State, Heading, Value, Comment);
                  end Put;

               begin
                  Scan_Table_Rows (Message, Put'Access);
               end;
            end if;

            declare
               procedure Put (Update : Property_Update_Type);

               ---------
               -- Put --
               ---------

               procedure Put (Update : Property_Update_Type) is
               begin
                  case Update.Property is
                     when Unrest_Property =>
                        Ada.Text_IO.Put_Line
                          (State.Local_Text
                             ("unrest-change",
                              Agrippa.Images.Image
                                (Natural (Old_Unrest_Level (Update))),
                              Agrippa.Images.Image
                                (Unrest_Change (Update)),
                              Agrippa.Images.Image
                                (Natural (New_Unrest_Level (Update)))));
                     when Event_Property =>
                        declare
                           Event : constant Agrippa.Events.Event_Type :=
                                     Agrippa.Messages.Event (Update);
                        begin
                           Ada.Text_IO.Put_Line (Event'Image & "!");
                        end;
                  end case;
               end Put;
            begin
               Scan_Property_Updates (Message, Put'Access);
            end;

         when Make_Proposal =>
            declare
               Count : Natural := 0;

               First_Consul : Senator_Id;

               Senator : constant Senator_Id :=
                           Get_Senator (Message);

               procedure Show_Proposal
                 (Proposal : Agrippa.Proposals.Proposal_Type);

               function Show_Proposal_Force
                 (Proposal : Agrippa.Proposals.Proposal_Type)
                  return String;

               -------------------
               -- Show_Proposal --
               -------------------

               procedure Show_Proposal
                 (Proposal : Agrippa.Proposals.Proposal_Type)
               is
                  use all type Agrippa.Proposals.Proposal_Category_Type;
               begin
                  Count := Count + 1;

                  case Proposal.Category is
                     when Office_Nomination =>

                        declare
                           Office : constant Office_Type :=
                                      Agrippa.Proposals.Office (Proposal);
                        begin
                           if Office in Rome_Consul | Field_Consul then
                              declare
                                 Nominee : constant Senator_Id :=
                                             Agrippa.Proposals.Nominee
                                               (Proposal);
                              begin
                                 if Count = 1 then
                                    First_Consul := Nominee;
                                 else
                                    Put (" ");
                                    Put_Line
                                      (State.Local_Text
                                         ("nominate-x-and-y-for-consuls",
                                          State.Senator_Name_And_Faction
                                            (First_Consul),
                                          State.Senator_Name_And_Faction
                                            (Nominee)));
                                 end if;
                              end;

                           else
                              Put (" ");
                              Put_Line
                                (State.Local_Text
                                   ("nominate-for-office",
                                    State.Senator_Name_And_Faction
                                      (Agrippa.Proposals.Nominee (Proposal)),
                                    State.Local_Text (Office'Image)));
                           end if;
                        end;

                     when others =>
                        if Count = 1 then
                           New_Line;
                        end if;

                        Set_Col (8);
                        Put (Agrippa.Images.Image (Count));
                        Put (". ");

                        case Proposal.Category is
                           when Recruitment =>
                              declare
                                 Ls : constant Legion_Count :=
                                        Agrippa.Proposals.Legions (Proposal);
                                 Fs : constant Fleet_Count :=
                                        Agrippa.Proposals.Fleets (Proposal);
                              begin
                                 if Ls > 0 and then Fs > 0 then
                                    Put (State.Local_Text
                                         ("recruit-legions-and-fleets",
                                            Agrippa.Images.Image
                                              (Natural (Ls), "legion"),
                                            Agrippa.Images.Image
                                              (Natural (Fs), "fleet")));
                                 elsif Ls > 0 then
                                    Put (State.Local_Text
                                         ("recruit-legions",
                                            Agrippa.Images.Image
                                              (Natural (Ls), "legion")));
                                 else
                                    Put (State.Local_Text
                                         ("recruit-fleets",
                                            Agrippa.Images.Image
                                              (Natural (Fs), "fleet")));
                                 end if;
                              end;
                           when Attack =>
                              Put (State.Local_Text
                                   ("attack-war-with",
                                      State.Senator_Name
                                        (Agrippa.Proposals.Commander
                                           (Proposal)),
                                      State.Local_Text
                                        (Agrippa.Cards.Wars.War
                                           (Agrippa.Proposals.War (Proposal))
                                         .Tag),
                                      Show_Proposal_Force (Proposal)));
--                                        Agrippa.Images.Image
--                                          (Natural
--                                          (Agrippa.Proposals.Regular_Legions
--                                                  (Proposal)),
--                                           "legion"),
--                                        Agrippa.Images.Image
--                                          (Natural
--                                             (Agrippa.Proposals.Fleets
--                                                  (Proposal)),
--                                           "fleet")));

                           when others =>
                              Put (Proposal.Category'Image);
                        end case;
                        New_Line;
                  end case;
               end Show_Proposal;

               -------------------------
               -- Show_Proposal_Force --
               -------------------------

               function Show_Proposal_Force
                 (Proposal : Agrippa.Proposals.Proposal_Type)
                  return String
               is
                  Reg : constant Legion_Count :=
                          Agrippa.Proposals.Regular_Legions (Proposal);
                  Vet : constant Legion_Index_Array :=
                          Agrippa.Proposals.Veteran_Legions (Proposal);
                  Flt : constant Fleet_Count :=
                          Agrippa.Proposals.Fleets (Proposal);
               begin
                  if Vet'Length = 0 then
                     if Flt = 0 then
                        return State.Local_Text
                          ("n-legions", Agrippa.Images.Image (Reg));
                     elsif Reg = 0 then
                        return State.Local_Text
                          ("n-fleets", Agrippa.Images.Image (Flt));
                     else
                        return State.Local_Text
                          ("legions-and-fleets",
                           Agrippa.Images.Image (Reg),
                           Agrippa.Images.Image (Flt));
                     end if;
                  else
                     declare
                        use Ada.Strings.Unbounded;
                        Vets : Unbounded_String;
                     begin
                        for Id of Vet loop
                           Vets := Vets
                             & (if Vets = "" then "" else " ")
                             & WL.Numerics.Roman.Roman_Image
                             (Positive (Id));
                        end loop;
                        return State.Local_Text
                          ("legions-veterans-and-fleets",
                           Agrippa.Images.Image (Reg),
                           To_String (Vets),
                           Agrippa.Images.Image (Flt));
                     end;
                  end if;
               end Show_Proposal_Force;

            begin
               Put
                 (State.Local_Text
                    ("senator-proposes",
                     (State.Local_Text
                          ("senator-of-faction",
                           State.Senator_Name (Senator),
                           State.Faction_Name
                             (State.Senator_Faction (Senator))))));
               Agrippa.Proposals.Scan_Proposals
                 (Agrippa.Messages.Proposals (Message),
                  Show_Proposal'Access);
            end;

         when Proposal_Vote =>
            null;

         when Attack =>

            Put_Line
              (State.Local_Text
                 ("senator-attacks-with",
                  State.Senator_Name
                    (Agrippa.Messages.Get_Senator (Message)),
                  State.Local_Text
                    (Agrippa.Cards.Wars.War
                         (War (Message)).Tag),
                  Agrippa.Images.Image
                    (Natural (Legions (Message)),
                     "legion"),
                  Agrippa.Images.Image
                    (Natural (Fleets (Message)),
                     "fleet")));

         when Player_Action =>

            case Get_Action (Message) is
               when Check_Rebellion =>

                  Put_Line
                    (State.Senator_Name (Get_Senator (Message))
                     & " returns to Rome and lays down his command");

               when Play_Card =>

                  Put_Line
                    (State.Faction_Name (Get_Faction (Message))
                     & " plays "
                     & State.Local_Text
                       (Agrippa.Cards.Card
                            (Get_Card (Message)).Tag)
                     & " on "
                     & State.Senator_Name
                       (Get_Senator (Message)));

            end case;

      end case;

   end Send_Message;

   -----------------------
   -- Send_Notification --
   -----------------------

   overriding procedure Send_Notification
     (Handler : Text_Notifier_Type;
      Text    : String)
   is
      pragma Unreferenced (Handler);
   begin
      Ada.Text_IO.Put_Line (Text);
   end Send_Notification;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Text_UI  : in out Text_UI_Type)
   is
      Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                   Agrippa.Scenarios.Get ("early-republic");
      Game     : Agrippa.Game.Game_Type renames Text_UI.Game;
   begin
      Game.Add_Faction ("Gladius");
      Game.Add_Faction ("Pila");
      Game.Add_Faction ("Aquarius");
      Game.Add_Faction ("Boni");
      Game.Add_Faction ("Agnus Dei");
      Game.Add_Faction ("Agricola");
      Game.Start (Scenario, WL.Localisation.To_Language ("en"),
                  Text_Notifier'Access);
      Agrippa.Players.Robots.Configure.Configure_Robots
        (Game, (1, 2, 3, 4, 5, 6));

      declare
         Phase_Number : Natural := 0;
         Step_Number  : Natural;
      begin
         for Faction in Faction_Id loop
            Phase_Number := Phase_Number + 1;
            Step_Number := 0;
            loop
               Step_Number := Step_Number + 1;
               Game.Set_Current_Activity (Phase_Number, Step_Number);
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

      for I in 1 .. 4 loop
         Text_UI.Start_Turn;

         declare
            Id : Agrippa.Phases.Sequence.Phase_Id :=
                   Agrippa.Phases.Sequence.First_Phase;
            Phase_Number : Natural := 0;
         begin

            loop
               Phase_Number := Phase_Number + 1;
               Game.Set_Current_Activity (Phase_Number, 0);

               Put_Phase (Game, Id, Agrippa.Phases.Sequence.Phase (Id));

               declare
                  Phase : constant Agrippa.Phases.Phase_Interface'Class :=
                            Agrippa.Phases.Sequence.Phase (Id);
                  State : Agrippa.Phases.Phase_State_Type'Class :=
                            Phase.Start (Game);
                  Step_Number : Natural := 0;
               begin

                  while not State.Is_Finished loop
                     declare
                        Step_Name : constant String :=
                                      Phase.Current_Step_Name (State, Game);
                     begin
                        if Step_Name /= "" then
                           Ada.Text_IO.New_Line;
                           Ada.Text_IO.Put_Line ("--- " & Step_Name);
                        end if;
                     end;

                     Step_Number := Step_Number + 1;
                     Game.Set_Current_Activity (Phase_Number, Step_Number);
                     Phase.Step (State, Game);

                  end loop;
               end;

               exit when Game.End_Of_Game;

               exit when Agrippa.Phases.Sequence.Is_Last (Id);

               Id := Agrippa.Phases.Sequence.Next_Phase (Id);

            end loop;

         end;

         exit when Game.End_Of_Game;

         Text_UI.State_Of_The_Republic;

      end loop;

      if Game.End_Of_Game then
         Ada.Text_IO.Put
           ("Game over: ");
         if Game.Has_Winner then
            Ada.Text_IO.Put_Line
              (Game.Faction_Name (Game.Winning_Faction) & " wins!");
         else
            Ada.Text_IO.Put_Line
              ("Everybody loses!");
         end if;
      end if;

      Game.Stop;

   end Start;

   ----------------
   -- Start_Turn --
   ----------------

   procedure Start_Turn
     (UI : in out Text_UI_Type'Class)
   is
      Game : Agrippa.Game.Game_Type renames UI.Game;
   begin
      Game.Start_Turn;
      Put_Field (Game, "Scenario", Agrippa.Scenarios.Show (Game.Scenario));
      Put_Field (Game, "Turn", Positive (Game.Current_Turn));
      if Game.Current_Turn = 1 then
         UI.State_Of_The_Republic;
      end if;
   end Start_Turn;

   ---------------------------
   -- State_Of_The_Republic --
   ---------------------------

   procedure State_Of_The_Republic
     (UI : in out Text_UI_Type'Class)
   is
      use Agrippa.Images;
      Game : Agrippa.Game.Game_Type renames UI.Game;
   begin
      Put_Heading ("State of the Republic");
      Put_Field (Game, "treasury-heading",
                 Natural (Game.Current_Treasury), "t");
      Put_Field (Game, "unrest-level", Natural (Game.Current_Unrest));
      Put_Field (Game, "legions",
                 Image (Game.Total_Legion_Count)
                 & ": " & Image (Game.Regular_Legion_Count)
                 & "reg");
      Put_Field (Game, "fleets",
                 Image (Game.Total_Fleet_Count));

      declare

         function Is_Active (War : War_Id) return Boolean
         is (Game.Get_War_State (War).Active);

         function Is_Inactive (War : War_Id) return Boolean
         is (Game.Get_War_State (War).Inactive);

         procedure Show_Wars
           (Field_Tag : String;
            Test      : not null access
              function (War : War_Id) return Boolean);

         ---------------
         -- Show_Wars --
         ---------------

         procedure Show_Wars
           (Field_Tag : String;
            Test      : not null access
              function (War : War_Id) return Boolean)
         is
            Match : constant War_Id_Array :=
                      Game.Matching_Wars (Test);
         begin
            if Match'Length = 0 then
               Put_Field
                 (Game, Field_Tag, ".");
            else
               for I in Match'Range loop
                  declare
                     use Agrippa.Cards.Wars;
                     War        : constant War_Card_Type'Class :=
                                    Agrippa.Cards.Wars.War (Match (I));
                     Field_Name : constant String :=
                                    (if I = Match'First
                                     then Field_Tag
                                     else "");
                  begin
                     Put_Field (Game, Field_Name,
                                Game.Local_Text (War.Tag)
                                & " "
                                & Image (War.Land_Strength)
                                & "/"
                                & Image (War.Fleet_Support)
                                & "/"
                                & Image (War.Fleet_Strength));
                  end;
               end loop;
            end if;
         end Show_Wars;

      begin
         Show_Wars ("active-wars", Is_Active'Access);
         Show_Wars ("inactive-wars", Is_Inactive'Access);
      end;

      Put_Field (Game, "cards-in-deck",
                 Image (Game.Forum_Deck.Remaining)
                 & "/"
                 & Image (Game.Forum_Deck.Remaining
                   + Game.Forum_Deck.Drawn));

      declare
         HRAO : constant Senator_Id := Game.Highest_Ranking_Available_Officer;
      begin
         Put_Field (Game, "hrao",
                    Game.Local_Text
                      ("senator-of-faction",
                       Game.Senator_Name (HRAO),
                       Game.Faction_Name (Game.Senator_Faction (HRAO))));
      end;

      Put_Heading ("Factions");

      for Id in Faction_Id loop
         declare
            Faction : constant Agrippa.Factions.Faction_Type'Class :=
                        Game.Faction (Id);
         begin
            if Faction.Active then
               Put_Faction (Game, Faction);
            end if;
         end;
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("*** Unaligned Senators");

      declare
         Curia : constant Senator_Id_Array :=
                   Game.Curia_Senators;
      begin
         if Curia'Length = 0 then
            Ada.Text_IO.Put_Line ("None");
         else
            Put_Senators (Game, Curia);
         end if;
         Ada.Text_IO.New_Line;
      end;

   end State_Of_The_Republic;

end Agrippa.UI.Text;
