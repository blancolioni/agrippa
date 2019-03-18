package body Agrippa.State.Legions is

   ------------
   -- Create --
   ------------

   procedure Create
     (Legion : in out Legion_State_Type'Class)
   is
   begin
      Legion.Created := True;
   end Create;

   ------------
   -- Deploy --
   ------------

   procedure Deploy
     (Legion  : in out Legion_State_Type'Class;
      War     : War_Id)
   is
   begin
      Legion.Deployed := True;
      Legion.War := War;
   end Deploy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Legion : in out Legion_State_Type'Class)
   is
   begin
      Legion.Created := False;
      Legion.Deployed := False;
      Legion.Veteran := False;
      Legion.Loyal := False;
   end Destroy;

   ------------------
   -- Make_Veteran --
   ------------------

   procedure Make_Veteran
     (Legion : in out Legion_State_Type'Class)
   is
   begin
      Legion.Veteran := True;
   end Make_Veteran;

   procedure Recall
     (Legion  : in out Legion_State_Type'Class)
   is
   begin
      Legion.Deployed := False;
   end Recall;

   -----------------
   -- Set_Loyalty --
   -----------------

   procedure Set_Loyalty
     (Legion : in out Legion_State_Type'Class;
      Senator : Senator_Id)
   is
   begin
      Legion.Loyal := True;
      Legion.Loyalty := Senator;
   end Set_Loyalty;

end Agrippa.State.Legions;
