with Agrippa.Cards.Leaders;

package Agrippa.State.Leaders is

   type Leader_State_Type is tagged private;

   function Id
     (State : Leader_State_Type'Class)
      return Leader_Id;

   function In_Play
     (Leader : Leader_State_Type'Class)
      return Boolean;

   function In_Curia
     (Leader : Leader_State_Type'Class)
      return Boolean;

   function Active
     (Leader : Leader_State_Type'Class)
      return Boolean;

   function War
     (Leader : Leader_State_Type'Class)
      return War_Id;

   function Strength
     (Leader : Leader_State_Type'Class)
      return Natural;

   function Disaster
     (Leader : Leader_State_Type'Class)
      return Agrippa.Dice.TDR_Range;

   function Stand_Off
     (Leader : Leader_State_Type'Class)
      return Agrippa.Dice.TDR_Range;

   procedure Set_In_Play
     (Leader : in out Leader_State_Type;
      Id      : Leader_Id)
     with Pre => not Leader.In_Play,
     Post => Leader.In_Play
     and then not Leader.In_Curia
     and then not Leader.Active
     and then Leader.Id = Id;

   procedure Set_In_Curia
     (Leader  : in out Leader_State_Type)
     with Pre => Leader.In_Play,
     Post => Leader.In_Play
     and then Leader.In_Curia
     and then not Leader.Active;

   procedure Set_Active
     (Leader  : in out Leader_State_Type)
     with Pre => Leader.In_Play,
     Post => Leader.In_Play
     and then not Leader.In_Curia
     and then Leader.Active;

   procedure Kill
     (Leader : in out Leader_State_Type)
     with Pre => Leader.In_Play and then Leader.In_Curia,
     Post => Leader.In_Play
     and then not Leader.In_Curia
     and then not Leader.Active;

private

   type Leader_State_Type is tagged
      record
         Id       : Leader_Id;
         In_Play  : Boolean := False;
         In_Curia : Boolean := False;
         Active   : Boolean := False;
         War      : War_Id;
      end record;

   function Leader
     (State : Leader_State_Type'Class)
      return Leader_Id
   is (State.Id);

   function Id
     (State : Leader_State_Type'Class)
      return Leader_Id
   is (State.Id);

   function In_Curia
     (Leader : Leader_State_Type'Class)
      return Boolean
   is (Leader.In_Curia);

   function In_Play
     (Leader : Leader_State_Type'Class)
      return Boolean
   is (Leader.In_Play);

   function Active
     (Leader : Leader_State_Type'Class)
      return Boolean
   is (Leader.Active);

   function War
     (Leader : Leader_State_Type'Class)
      return War_Id
   is (Leader.War);

   function Strength
     (Leader : Leader_State_Type'Class)
      return Natural
   is (Agrippa.Cards.Leaders.Leader (Leader.Id).Strength);

   function Disaster
     (Leader : Leader_State_Type'Class)
      return Agrippa.Dice.TDR_Range
   is (Agrippa.Cards.Leaders.Leader (Leader.Id).Disaster);

   function Stand_Off
     (Leader : Leader_State_Type'Class)
      return Agrippa.Dice.TDR_Range
   is (Agrippa.Cards.Leaders.Leader (Leader.Id).Stand_Off);

end Agrippa.State.Leaders;
