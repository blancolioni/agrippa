with Agrippa.Scenarios;

package Agrippa.Cards is

   type Card_Class is
     (Concession_Card,
      Intrigue_Card,
      Leader_Card,
      Senator_Card,
      Statesman_Card,
      War_Card);

   type Card_Type is abstract tagged private;

   function Id (Card : Card_Type'Class) return Card_Id;
   function Tag (Card : Card_Type'Class) return String;
   function Class (Card : Card_Type'Class) return Card_Class;

   function Scenario
     (Card : Card_Type'Class)
      return Agrippa.Scenarios.Scenario_Type;

   function Keep
     (Card : Card_Type'Class)
      return Boolean;

   function Card (Id : Card_Id) return Card_Type'Class;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Card_Id
     with Pre => Exists (Tag);

private

   type Card_Tag is access constant String;

   type Card_Type is abstract tagged
      record
         Id       : Card_Id;
         Tag      : Card_Tag;
         Scenario : Agrippa.Scenarios.Scenario_Type;
         Class    : Card_Class;
         Keep     : Boolean;
      end record;

   function Id (Card : Card_Type'Class) return Card_Id
   is (Card.Id);

   function Tag (Card : Card_Type'Class) return String
   is (Card.Tag.all);

   function Scenario
     (Card : Card_Type'Class)
      return Agrippa.Scenarios.Scenario_Type
   is (Card.Scenario);

   function Class (Card : Card_Type'Class) return Card_Class
   is (Card.Class);

   function Keep
     (Card : Card_Type'Class)
      return Boolean
   is (Card.Keep);

   procedure New_Card
     (Card : in out Card_Type'Class);

   type War_Id_Array_Access is access constant War_Id_Array;

end Agrippa.Cards;
