package Agrippa.State.Fleets is

   type Fleet_State_Type is
     new Military_Type with private;

   procedure Create (Fleet : in out Fleet_State_Type'Class)
     with Pre => not Fleet.Created,
     Post => Fleet.Created and then not Fleet.Deployed;

   procedure Destroy (Fleet : in out Fleet_State_Type'Class)
     with Pre => Fleet.Created,
     Post => not Fleet.Created;

   procedure Deploy
     (Fleet : in out Fleet_State_Type'Class;
      War   : War_Id)
     with Pre => Fleet.Created,
     Post => Fleet.Deployed
     and then Fleet.War = War;

   procedure Recall
     (Fleet : in out Fleet_State_Type'Class)
     with Pre => Fleet.Created and then Fleet.Deployed,
     Post => not Fleet.Deployed;

private

   type Fleet_State_Type is
     new Military_Type with
      record
         null;
      end record;

end Agrippa.State.Fleets;
