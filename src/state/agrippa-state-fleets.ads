package Agrippa.State.Fleets is

   type Fleet_State_Type is tagged private;

   function Created (Fleet : Fleet_State_Type'Class) return Boolean;
   function Deployed (Fleet : Fleet_State_Type'Class) return Boolean;

   function War
     (Fleet : Fleet_State_Type'Class)
      return War_Id
     with Pre => Fleet.Deployed;

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

   type Fleet_State_Type is tagged
      record
         Created  : Boolean := False;
         Deployed : Boolean := False;
         War      : War_Id;
      end record;

   function Created (Fleet : Fleet_State_Type'Class) return Boolean
   is (Fleet.Created);

   function Deployed (Fleet : Fleet_State_Type'Class) return Boolean
   is (Fleet.Deployed);

   function War
     (Fleet : Fleet_State_Type'Class)
      return War_Id
   is (Fleet.War);

end Agrippa.State.Fleets;
