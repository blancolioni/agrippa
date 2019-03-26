package Agrippa.Cards.Leaders is

   type Leader_Card_Type is
     new Card_Type with private;

   function Leader (Card : Leader_Card_Type'Class) return Leader_Id;
   function Leader (Id : Leader_Id) return Leader_Card_Type'Class;

   function All_Leaders
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Leader_Id_Array;

   function Strength (Card : Leader_Card_Type'Class) return Natural;

   function Disaster
     (Card : Leader_Card_Type'Class)
      return TDR_Range;

   function Stand_Off
     (Card : Leader_Card_Type'Class)
      return TDR_Range;

   function Matching_Wars
     (Card : Leader_Card_Type'Class)
      return War_Id_Array;

private

   type Leader_Card_Type is
     new Card_Type with
      record
         L_Id           : Leader_Id;
         Strength       : Natural;
         Disaster       : TDR_Range;
         Stand_Off      : TDR_Range;
         Matching_Wars  : access constant War_Id_Array;
      end record;

   function Leader (Card : Leader_Card_Type'Class) return Leader_Id
   is (Card.L_Id);

   function Strength (Card : Leader_Card_Type'Class) return Natural
   is (Card.Strength);

   function Disaster
     (Card : Leader_Card_Type'Class)
      return TDR_Range
   is (Card.Disaster);

   function Stand_Off
     (Card : Leader_Card_Type'Class)
      return TDR_Range
   is (Card.Stand_Off);

   function Matching_Wars
     (Card : Leader_Card_Type'Class)
      return War_Id_Array
   is (Card.Matching_Wars.all);

   procedure New_Leader
     (Card : in out Leader_Card_Type'Class);

end Agrippa.Cards.Leaders;
