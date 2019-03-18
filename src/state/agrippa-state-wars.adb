package body Agrippa.State.Wars is

   -------------
   -- Discard --
   -------------

   procedure Discard
     (War : in out War_State_Type)
   is
   begin
      War.Active := False;
      War.Discarded := True;
      War.Prosecuted := False;
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
   end On_Drawn;

   ---------------
   -- Prosecute --
   ---------------

   procedure Prosecute
     (War : in out War_State_Type)
   is
   begin
      War.Active := True;
      War.Prosecuted := True;
   end Prosecute;

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
end Agrippa.State.Wars;
