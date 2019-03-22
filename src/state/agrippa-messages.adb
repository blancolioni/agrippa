with Agrippa.Images;

package body Agrippa.Messages is

   -------------------------
   -- Add_Property_Update --
   -------------------------

   procedure Add_Property_Update
     (Message : in out Message_Type;
      Update  : Property_Update_Type)
   is
   begin
      Message.Property_Updates.Append (Update);
   end Add_Property_Update;

   -------------------
   -- Add_Table_Row --
   -------------------

   procedure Add_Table_Row
     (Message : in out Message_Type;
      Heading : String;
      Value   : String;
      Comment : String := "")
   is
      function "+" (X : String) return Ada.Strings.Unbounded.Unbounded_String
                    renames Ada.Strings.Unbounded.To_Unbounded_String;
   begin
      Message.Has_Table := True;
      Message.Table.Append
        (Table_Row'
           (Heading => +Heading,
            Value   => +Value,
            Comment => +Comment));
   end Add_Table_Row;

   -------------------
   -- Add_Table_Row --
   -------------------

   procedure Add_Table_Row
     (Message : in out Message_Type;
      Heading : String;
      Value   : Integer;
      Comment : String := "")
   is
   begin
      Add_Table_Row (Message, Heading, Agrippa.Images.Image (Value), Comment);
   end Add_Table_Row;

   -------------------------------
   -- Add_Transfer_From_Senator --
   -------------------------------

   procedure Add_Transfer_From_Senator
     (Message      : in out Faction_Transfers_Message;
      From_Senator : Senator_Id;
      Amount       : Talents)
   is
   begin
      Message.Transfer_List.Append
        (Single_Transfer'
           (From_Faction => False,
            To_Faction   => True,
            From_Senator => From_Senator,
            To_Senator   => <>,
            Amount       => Amount));
   end Add_Transfer_From_Senator;

   -----------------------------
   -- Add_Transfer_To_Senator --
   -----------------------------

   procedure Add_Transfer_To_Senator
     (Message    : in out Faction_Transfers_Message;
      To_Senator : Senator_Id;
      Amount     : Talents)
   is
   begin
      Message.Transfer_List.Append
        (Single_Transfer'
           (From_Faction => True,
            To_Faction   => False,
            From_Senator => <>,
            To_Senator   => To_Senator,
            Amount       => Amount));
   end Add_Transfer_To_Senator;

   ------------
   -- Attack --
   ------------

   function Attack
     (War       : War_Id)
      return Agrippa.Messages.Message_Type
   is
   begin
      return Response : Message_Type (Attack) do
         Response.War := War;
      end return;
   end Attack;

   -----------------
   -- Attack_With --
   -----------------

   function Attack_With
     (Message   : Message_Type;
      Commander : Senator_Id;
      Legions   : Legion_Count;
      Fleets    : Fleet_Count)
      return Message_Type
   is
   begin
      return Response : Message_Type := Message do
         Response.Senator := Commander;
         Response.Has_Senator := True;
         Response.Legions := Legions;
         Response.Fleets := Fleets;
      end return;
   end Attack_With;

   ---------------------
   -- Attract_Knights --
   ---------------------

   function Attract_Knights
     (Faction : Faction_Id)
      return Attract_Knights_Message
   is
   begin
      return Message : Attract_Knights_Message do
         Message.Has_Faction := True;
         Message.Faction := Faction;
      end return;
   end Attract_Knights;

   ---------------------
   -- Attract_Knights --
   ---------------------

   function Attract_Knights
     (Message : Attract_Knights_Message;
      Senator : Senator_Id;
      Spend   : Talents)
      return Attract_Knights_Message
   is
   begin
      return Response : Attract_Knights_Message := Message do
         Response.Has_Senator := True;
         Response.Senator := Senator;
         Response.Money := Spend;
      end return;
   end Attract_Knights;

   ---------------------
   -- Attract_Knights --
   ---------------------

   function Attract_Knights
     (Message : Attract_Knights_Message;
      Roll    : Positive;
      Success : Boolean)
      return Attract_Knights_Message
   is
   begin
      return Response : Attract_Knights_Message := Message do
         Response.Roll := Roll;
         Response.Success := Success;
      end return;
   end Attract_Knights;

   -------------------
   -- Empty_Message --
   -------------------

   function Empty_Message return Message_Type is
   begin
      return Message : Message_Type (Empty_Message) do
         pragma Unmodified (Message);
      end return;
   end Empty_Message;

   ----------------------
   -- Event_Occurrence --
   ----------------------

   function Event_Occurrence
     (Event     : Agrippa.Events.Event_Type)
      return Event_Update_Type
   is
   begin
      return Update : Event_Update_Type do
         Update.Event := Event;
         Update.Change := 1;
      end return;
   end Event_Occurrence;

   -------------------------
   -- Event_Status_Change --
   -------------------------

   function Event_Status_Change
     (Event     : Agrippa.Events.Event_Type;
      Old_Level : Natural;
      New_Level : Natural)
      return Event_Update_Type
   is
   begin
      return Update : Event_Update_Type do
         Update.Event := Event;
         Update.Old_Level := Old_Level;
         Update.New_Level := New_Level;
         Update.Change := Update.New_Level - Update.Old_Level;
      end return;
   end Event_Status_Change;

   -----------------------
   -- Faction_Transfers --
   -----------------------

   function Faction_Transfers
     (Faction : Faction_Id)
      return Faction_Transfers_Message
   is
   begin
      return Message : Faction_Transfers_Message do
         Message.Has_Faction := True;
         Message.Faction := Faction;
      end return;
   end Faction_Transfers;

   ---------------------
   -- Initiative_Roll --
   ---------------------

   function Initiative_Roll
     (Faction : Faction_Id)
      return Initiative_Roll_Message
   is
   begin
      return Message : Initiative_Roll_Message do
         Message.Has_Faction := True;
         Message.Faction     := Faction;
      end return;
   end Initiative_Roll;

   ---------------------
   -- Initiative_Roll --
   ---------------------

   function Initiative_Roll
     (Message : Initiative_Roll_Message;
      Roll    : Agrippa.Dice.DR_Range;
      Event   : Agrippa.Dice.TDR_Range)
      return Initiative_Roll_Message
   is
   begin
      return Response : Initiative_Roll_Message := Message do
         Response.Roll := Roll;
         Response.Event_Roll := Event;
      end return;
   end Initiative_Roll;

   ---------------------
   -- Initiative_Roll --
   ---------------------

   function Initiative_Roll
     (Message : Initiative_Roll_Message;
      Roll    : Agrippa.Dice.DR_Range;
      Card    : Card_Id)
      return Initiative_Roll_Message
   is
   begin
      return Response : Initiative_Roll_Message := Message do
         Response.Roll := Roll;
         Response.Drawn_Card := Card;
      end return;
   end Initiative_Roll;

   ------------------------------
   -- Make_Consular_Nomination --
   ------------------------------

   function Make_Consular_Nomination
     (Senator  : Senator_Id;
      Faction  : Faction_Id)
      return Make_Proposal_Message
   is
   begin
      return Message : Make_Proposal_Message do
         Message.Has_Senator := True;
         Message.Senator := Senator;
         Message.Has_Faction := True;
         Message.Faction := Faction;
         Message.Allowed_Categories (Agrippa.Proposals.Office_Nomination) :=
           True;
         Message.Allowed_Offices (Rome_Consul) := True;
         Message.Allowed_Offices (Field_Consul) := True;
      end return;
   end Make_Consular_Nomination;

   ----------------------------
   -- Make_Office_Nomination --
   ----------------------------

   function Make_Office_Nomination
     (Senator  : Senator_Id;
      Faction  : Faction_Id;
      Office   : Office_Type)
      return Make_Proposal_Message
   is
   begin
      return Message : Make_Proposal_Message do
         Message.Has_Senator := True;
         Message.Senator := Senator;
         Message.Has_Faction := True;
         Message.Faction := Faction;
         Message.Allowed_Categories (Agrippa.Proposals.Office_Nomination) :=
           True;
         Message.Allowed_Offices (Office) := True;
      end return;
   end Make_Office_Nomination;

   -------------------
   -- Make_Proposal --
   -------------------

   function Make_Proposal
     (Senator  : Senator_Id;
      Faction  : Faction_Id;
      Category : Agrippa.Proposals.Proposal_Category_Type)
      return Make_Proposal_Message
   is
   begin
      return Message : Make_Proposal_Message do
         Message.Has_Senator := True;
         Message.Senator := Senator;
         Message.Has_Faction := True;
         Message.Faction := Faction;
         Message.Allowed_Categories (Category) := True;
      end return;
   end Make_Proposal;

   -------------------
   -- Make_Proposal --
   -------------------

   function Make_Proposal
     (Senator    : Senator_Id;
      Faction    : Faction_Id;
      Categories : Agrippa.Proposals.Proposal_Category_Array)
      return Make_Proposal_Message
   is
   begin
      return Message : Make_Proposal_Message do
         Message.Has_Senator := True;
         Message.Senator := Senator;
         Message.Has_Faction := True;
         Message.Faction := Faction;
         for Category of Categories loop
            Message.Allowed_Categories (Category) := True;
         end loop;
      end return;
   end Make_Proposal;

   -------------------
   -- Make_Proposal --
   -------------------

   function Make_Proposal
     (Message  : Make_Proposal_Message;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
      return Make_Proposal_Message
   is
   begin
      return Response : Make_Proposal_Message := Message do
         Response.Proposal := Proposal;
      end return;
   end Make_Proposal;

   --------------------
   -- Mortality_None --
   --------------------

   function Mortality_None
     (Roll    : Positive)
      return Message_Type
   is
   begin
      return Message : Message_Type := Mortality_Roll do
         Message.Roll := Roll;
         Message.No_Deaths := True;
      end return;
   end Mortality_None;

   --------------------
   -- Mortality_Roll --
   --------------------

   function Mortality_Roll return Message_Type is
   begin
      return Message : constant Message_Type := (Mortality_Roll, others => <>);
   end Mortality_Roll;

   --------------------------
   -- Mortality_Roll_Twice --
   --------------------------

   function Mortality_Roll_Twice
     (Roll    : Positive)
      return Message_Type
   is
   begin
      return Message : Message_Type := Mortality_Roll do
         Message.Roll := Roll;
         Message.Roll_Twice := True;
      end return;
   end Mortality_Roll_Twice;

   ----------------------------
   -- Mortality_Senator_Dies --
   ----------------------------

   function Mortality_Senator_Dies
     (Roll    : Positive;
      Senator : Senator_Id)
      return Message_Type
   is
   begin
      return Message : Message_Type := Mortality_Roll do
         Message.Has_Senator := True;
         Message.Senator := Senator;
         Message.Roll := Roll;
      end return;
   end Mortality_Senator_Dies;

   ------------------------
   -- Persuasion_Attempt --
   ------------------------

   function Persuasion_Attempt
     (Faction : Faction_Id)
      return Persuasion_Attempt_Message
   is
   begin
      return Message : Persuasion_Attempt_Message do
         Message.Has_Faction := True;
         Message.Faction := Faction;
      end return;
   end Persuasion_Attempt;

   ------------------------
   -- Persuasion_Attempt --
   ------------------------

   function Persuasion_Attempt
     (Message : Persuasion_Attempt_Message;
      Senator : Senator_Id;
      Target  : Senator_Id;
      Bribe   : Talents)
      return Persuasion_Attempt_Message
   is
   begin
      return Response : Persuasion_Attempt_Message := Message do
         Response.Has_Senator := True;
         Response.Senator := Senator;
         Response.Target := Target;
         Response.Money := Bribe;
      end return;
   end Persuasion_Attempt;

   ---------------------
   -- Population_Roll --
   ---------------------

   function Population_Roll return Message_Type is
   begin
      return Message : constant Message_Type :=
        (Population_Roll, others => <>);
   end Population_Roll;

   ----------------------
   -- Proposal_Offices --
   ----------------------

   function Proposal_Offices
     (Message  : Make_Proposal_Message)
      return Office_Array
   is
      Result : Office_Array (1 .. Message.Allowed_Offices'Length);
      Count  : Natural := 0;
   begin
      for Office in Office_Type loop
         if Message.Allowed_Offices (Office) then
            Count := Count + 1;
            Result (Count) := Office;
         end if;
      end loop;
      return Result (1 .. Count);
   end Proposal_Offices;

   ---------------------------
   -- Scan_Property_Updates --
   ---------------------------

   procedure Scan_Property_Updates
     (Message : Message_Type;
      Process : not null access
        procedure (Update : Property_Update_Type))
   is
   begin
      for Update of Message.Property_Updates loop
         Process (Update);
      end loop;
   end Scan_Property_Updates;

   ---------------------
   -- Scan_Table_Rows --
   ---------------------

   procedure Scan_Table_Rows
     (Message : Message_Type;
      Process : not null access
        procedure (Heading, Value, Comment : String))
   is
      function "-" (X : Ada.Strings.Unbounded.Unbounded_String) return String
                    renames Ada.Strings.Unbounded.To_String;
   begin
      for Item of Message.Table loop
         Process (-Item.Heading, -Item.Value, -Item.Comment);
      end loop;
   end Scan_Table_Rows;

   --------------------
   -- Scan_Transfers --
   --------------------

   procedure Scan_Transfers
     (Message             : Faction_Transfers_Message;
      On_Faction_Treasury : not null access
        procedure (Take   : Boolean;
                   Amount : Talents);
      On_Senator_Treasury : not null access
        procedure (Senator : Senator_Id;
                   Take    : Boolean;
                   Amount  : Talents))
   is
   begin
      for Transfer of Message.Transfer_List loop
         if Transfer.From_Faction then
            On_Faction_Treasury (True, Transfer.Amount);
         else
            On_Senator_Treasury (Transfer.From_Senator, True, Transfer.Amount);
         end if;
         if Transfer.To_Faction then
            On_Faction_Treasury (False, Transfer.Amount);
         else
            On_Senator_Treasury (Transfer.To_Senator, False, Transfer.Amount);
         end if;
      end loop;
   end Scan_Transfers;

   -------------------
   -- Unrest_Change --
   -------------------

   function Unrest_Change
     (Old_Level : Unrest_Level;
      Change    : Integer;
      New_Level : Unrest_Level)
      return Unrest_Update_Type
   is
   begin
      return Update : Unrest_Update_Type do
         Update.Old_Level := Natural (Old_Level);
         Update.New_Level := Natural (New_Level);
         Update.Change := Change;
      end return;
   end Unrest_Change;

end Agrippa.Messages;
