package body Agrippa.State.Fleets is

   ------------
   -- Create --
   ------------

   procedure Create
     (Fleet : in out Fleet_State_Type'Class)
   is
   begin
      Fleet.Created := True;
      Fleet.Deployed := False;
   end Create;

   ------------
   -- Deploy --
   ------------

   procedure Deploy
     (Fleet : in out Fleet_State_Type'Class;
      War   : War_Id)
   is
   begin
      Fleet.Deployed := True;
      Fleet.War := War;
   end Deploy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Fleet : in out Fleet_State_Type'Class)
   is
   begin
      Fleet.Created := False;
      Fleet.Deployed := False;
   end Destroy;

   ------------
   -- Recall --
   ------------

   procedure Recall
     (Fleet : in out Fleet_State_Type'Class)
   is
   begin
      Fleet.Deployed := False;
   end Recall;

end Agrippa.State.Fleets;
