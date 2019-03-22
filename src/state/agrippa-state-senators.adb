with Agrippa.Cards.Senators;

package body Agrippa.State.Senators is

   -------------------
   -- Add_Influence --
   -------------------

   procedure Add_Influence
     (Senator : in out Senator_State_Type;
      Infl    : Influence_Range)
   is
   begin
      Senator.Influence :=
        Senator.Influence
          + Influence_Range'Min
        (Infl,
         Influence_Range'Last - Senator.Influence);
   end Add_Influence;

   ----------------
   -- Add_Knight --
   ----------------

   procedure Add_Knight
     (Senator : in out Senator_State_Type'Class)
   is
   begin
      Senator.Knights := Senator.Knights + 1;
   end Add_Knight;

   -----------------
   -- Add_Talents --
   -----------------

   procedure Add_Talents
     (Senator : in out Senator_State_Type'Class;
      Count   : Talents)
   is
   begin
      Senator.Set_Treasury (Senator.Treasury + Count);
   end Add_Talents;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Senator    : in out Senator_State_Type;
      Concession : Agrippa.Cards.Concessions.Concession_Card_Type'Class)
   is
   begin
      Senator.Concessions.Append
        (Agrippa.Cards.Concessions.Concession_Card_Type (Concession));
   end Assign;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Senator : in out Senator_State_Type;
      War     : War_Id)
   is
   begin
      Senator.Leading_Army := True;
      Senator.War := War;
   end Attack;

   -----------------------
   -- Change_Popularity --
   -----------------------

   procedure Change_Popularity
     (Senator : in out Senator_State_Type;
      Pop     : Popularity_Range)
   is
      New_Pop : constant Integer :=
                  Integer (Senator.Popularity) + Integer (Pop);
   begin
      Senator.Popularity :=
        Popularity_Range
          (if New_Pop < -9 then -9
           elsif New_Pop > 9 then 9
           else Popularity_Range (New_Pop));
   end Change_Popularity;

   ------------------
   -- Clear_Office --
   ------------------

   procedure Clear_Office
     (Senator : in out Senator_State_Type'Class)
   is
   begin
      Senator.Has_Office := False;
   end Clear_Office;

   ----------
   -- Kill --
   ----------

   procedure Kill
     (Senator : in out Senator_State_Type;
      Leader  : Boolean)
   is
   begin
      Senator :=
        (Id          => Senator.Id,
         In_Play     => True,
         Has_Faction => Leader,
         Faction     => Senator.Faction,
         others      => <>);
   end Kill;

   -------------
   -- Oratory --
   -------------

   function Oratory
     (Senator : Senator_State_Type'Class)
      return Attribute_Range
   is
   begin
      return Agrippa.Cards.Senators.Senator (Senator.Id).Oratory;
   end Oratory;

   --------------------
   -- Remove_Talents --
   --------------------

   procedure Remove_Talents
     (Senator : in out Senator_State_Type'Class;
      Count   : Talents)
   is
   begin
      Senator.Set_Treasury (Senator.Treasury - Count);
   end Remove_Talents;

   -----------------
   -- Set_Faction --
   -----------------

   procedure Set_Faction
     (Senator : in out Senator_State_Type'Class;
      Faction : Faction_Id)
   is
   begin
      Senator.In_Curia := False;
      Senator.Has_Faction := True;
      Senator.Faction := Faction;
   end Set_Faction;

   ------------------
   -- Set_In_Curia --
   ------------------

   procedure Set_In_Curia
     (Senator : in out Senator_State_Type)
   is
   begin
      Senator.In_Curia := True;
   end Set_In_Curia;

   -----------------
   -- Set_In_Play --
   -----------------

   procedure Set_In_Play
     (Senator : in out Senator_State_Type;
      Id      : Senator_Id)
   is
      Base_Influence : constant Attribute_Range :=
                         Agrippa.Cards.Senators.Senator (Id).Influence;
   begin
      Senator :=
        (Id        => Id,
         In_Play   => True,
         Influence =>
           Influence_Range (Base_Influence),
         others    => <>);
   end Set_In_Play;

   ----------------
   -- Set_Office --
   ----------------

   procedure Set_Office
     (Senator : in out Senator_State_Type'Class;
      Office  : Office_Type)
   is
   begin
      Senator.Has_Office := True;
      Senator.Office := Office;
      Senator.Influence := Senator.Influence + Influence_Gain (Office);
      if Office in Consular_Office then
         Senator.Prior_Consul := True;
      end if;
   end Set_Office;

   ------------------
   -- Set_Treasury --
   ------------------

   procedure Set_Treasury
     (Senator : in out Senator_State_Type'Class;
      Count   : Talents)
   is
   begin
      Senator.Treasury := Count;
   end Set_Treasury;

   -----------
   -- Votes --
   -----------

   function Votes
     (Senator : Senator_State_Type'Class)
      return Vote_Count
   is
      Base : constant Vote_Count :=
               Vote_Count (Senator.Oratory);
   begin
      return Base + Vote_Count (Senator.Knights);
   end Votes;

end Agrippa.State.Senators;
