package Agrippa.Cards.Senators is

   type Senator_Card_Type is
     new Card_Type with private;

   function Military
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range;

   function Oratory
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range;

   function Loyalty
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range;

   function Influence
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range;

   function Senator
     (Id : Senator_Id)
     return Senator_Card_Type'Class;

   function Senator
     (Card : Senator_Card_Type'Class)
      return Senator_Id;

   function All_Senators return Senator_Id_Array;

private

   type Senator_Card_Type is
     new Card_Type with
      record
         S_Id      : Senator_Id;
         Military  : Attribute_Range;
         Oratory   : Attribute_Range;
         Loyalty   : Attribute_Range;
         Influence : Attribute_Range;
      end record;

   function Military
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range
   is (Senator.Military);

   function Oratory
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range
   is (Senator.Oratory);

   function Loyalty
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range
   is (Senator.Loyalty);

   function Influence
     (Senator : Senator_Card_Type'Class)
      return Attribute_Range
   is (Senator.Influence);

   function Senator
     (Card : Senator_Card_Type'Class)
      return Senator_Id
   is (Card.S_Id);

   procedure New_Senator
     (Card : in out Senator_Card_Type'Class);

end Agrippa.Cards.Senators;
