package body Agrippa.State.Wars is

   --------------
   -- Activate --
   --------------

   procedure Activate
     (War : in out War_State_Type)
   is
   begin
      War.Imminent := False;
      War.Active := True;
   end Activate;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (War : in out War_State_Type)
   is
   begin
      War.Active := True;
      War.Unprosecuted := False;
   end Attack;

   -------------
   -- Discard --
   -------------

   procedure Discard
     (War : in out War_State_Type)
   is
   begin
      War.Active := False;
      War.Discarded := True;
      War.Unprosecuted := False;
   end Discard;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (War     : in out War_State_Type'Class;
      Id      : War_Id;
      In_Play : Boolean)
   is
   begin
      War.Id := Id;
      War.In_Play := In_Play;
   end Initialize;

   --------------
   -- On_Drawn --
   --------------

   procedure On_Drawn
     (War : in out War_State_Type)
   is
   begin
      War.Drawn := True;
      War.Active := Agrippa.Cards.Wars.War (War.Id).Active;
      War.Unprosecuted := False;
   end On_Drawn;

   ------------------
   -- Start_Combat --
   ------------------

   procedure Start_Combat
     (War : in out War_State_Type)
   is
   begin
      War.Unprosecuted := False;
   end Start_Combat;

   ---------
   -- Tag --
   ---------

   function Tag
     (State : War_State_Type'Class)
      return String
   is
   begin
      return Agrippa.Cards.Wars.War (State.Id).Tag;
   end Tag;

   ------------------
   -- Unprosecuted --
   ------------------

   procedure Unprosecuted
     (War : in out War_State_Type)
   is
   begin
      War.Unprosecuted := True;
   end Unprosecuted;

end Agrippa.State.Wars;
