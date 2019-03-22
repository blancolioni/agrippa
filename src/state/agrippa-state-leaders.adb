package body Agrippa.State.Leaders is

   ----------
   -- Kill --
   ----------

   procedure Kill
     (Leader : in out Leader_State_Type)
   is
   begin
      Leader.Active := False;
      Leader.In_Curia := False;
   end Kill;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Leader  : in out Leader_State_Type)
   is
   begin
      Leader.Active := True;
   end Set_Active;

   ------------------
   -- Set_In_Curia --
   ------------------

   procedure Set_In_Curia
     (Leader  : in out Leader_State_Type)
   is
   begin
      Leader.In_Curia := True;
   end Set_In_Curia;

   -----------------
   -- Set_In_Play --
   -----------------

   procedure Set_In_Play
     (Leader : in out Leader_State_Type;
      Id      : Leader_Id)
   is
   begin
      Leader := Leader_State_Type'
        (Id       => Id,
         In_Play  => True,
         In_Curia => False,
         Active   => False,
         War      => <>);
   end Set_In_Play;

end Agrippa.State.Leaders;
