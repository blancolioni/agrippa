package Agrippa.Cards.Statesmen is

   type Statesman_Card_Type is
     new Card_Type with private;

   function Military
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range;

   function Oratory
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range;

   function Loyalty
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range;

   function Influence
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range;

   function Family
     (Statesman : Statesman_Card_Type'Class)
      return Senator_Id;

   function Statesman
     (Id : Statesman_Id)
     return Statesman_Card_Type'Class;

   function All_Statesmen
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Statesman_Id_Array;

private

   type Statesman_Card_Type is
     new Card_Type with
      record
         Family    : Senator_Id;
         Military  : Attribute_Range;
         Oratory   : Attribute_Range;
         Loyalty   : Attribute_Range;
         Influence : Attribute_Range;
      end record;

   function Military
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range
   is (Statesman.Military);

   function Oratory
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range
   is (Statesman.Oratory);

   function Loyalty
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range
   is (Statesman.Loyalty);

   function Influence
     (Statesman : Statesman_Card_Type'Class)
      return Attribute_Range
   is (Statesman.Influence);

   function Family
     (Statesman : Statesman_Card_Type'Class)
      return Senator_Id
   is (Statesman.Family);

   procedure New_Statesman
     (Card : in out Statesman_Card_Type'Class);

end Agrippa.Cards.Statesmen;
