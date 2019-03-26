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

   function Voids_DS
     (Statesman : Statesman_Card_Type'Class;
      War       : War_Id)
      return Boolean;

   function Statesman
     (Id : Statesman_Id)
     return Statesman_Card_Type'Class;

   function Statesman
     (Card : Statesman_Card_Type'Class)
      return Statesman_Id;

   function All_Statesmen
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Statesman_Id_Array;

private

   type Statesman_Card_Type is
     new Card_Type with
      record
         S_Id      : Statesman_Id;
         Family    : Senator_Id;
         Military  : Attribute_Range;
         Oratory   : Attribute_Range;
         Loyalty   : Attribute_Range;
         Influence : Attribute_Range;
         Voids_DS  : access constant War_Id_Array;
      end record;

   function Statesman
     (Card : Statesman_Card_Type'Class)
      return Statesman_Id
   is (Card.S_Id);

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

   function Voids_DS
     (Statesman : Statesman_Card_Type'Class;
      War       : War_Id)
      return Boolean
   is (for some X of Statesman.Voids_DS.all => X = War);

end Agrippa.Cards.Statesmen;
