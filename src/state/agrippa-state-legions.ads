package Agrippa.State.Legions is

   type Legion_State_Type is
     new Military_Type with private;

   function Loyal (Legion : Legion_State_Type'Class) return Boolean;

   function Loyalty
     (Legion : Legion_State_Type'Class)
      return Senator_Id
     with Pre => Legion.Loyal;

   procedure Create (Legion : in out Legion_State_Type'Class)
     with Pre => not Legion.Created,
     Post => Legion.Created and then not Legion.Deployed
     and then not Legion.Veteran and then not Legion.Loyal;

   procedure Destroy (Legion : in out Legion_State_Type'Class)
     with Pre => Legion.Created,
       Post => not Legion.Created;

   procedure Make_Veteran (Legion : in out Legion_State_Type'Class)
     with Pre => not Legion.Veteran,
       Post => Legion.Veteran;

   procedure Set_Loyalty
     (Legion : in out Legion_State_Type'Class;
      Senator : Senator_Id)
     with Pre => not Legion.Loyal,
     Post => Legion.Loyal
     and then Legion.Loyalty = Senator;

   procedure Deploy
     (Legion  : in out Legion_State_Type'Class;
      War     : War_Id)
     with Pre => Legion.Created,
     Post => Legion.Deployed
     and then Legion.War = War;

   procedure Recall
     (Legion  : in out Legion_State_Type'Class)
     with Pre => Legion.Created and then Legion.Deployed,
     Post => not Legion.Deployed;

private

   type Legion_State_Type is
     new Military_Type with
      record
         Loyal    : Boolean := False;
         Loyalty  : Senator_Id;
      end record;

   function Loyal (Legion : Legion_State_Type'Class) return Boolean
   is (Legion.Loyal);

   function Loyalty
     (Legion : Legion_State_Type'Class)
      return Senator_Id
   is (Legion.Loyalty);

end Agrippa.State.Legions;
