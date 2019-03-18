package Agrippa.Cards.Intrigue is

   type Intrigue_Card is
     (Tribune, Blackmail, Influence_Peddling,
      Seduction, Assassin, Secret_Bodyguard);

   type Intrigue_Card_Type is
     new Card_Type with private;

   function Intrigue (Id : Intrigue_Id) return Intrigue_Card_Type'Class;

   function All_Intrigues
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Intrigue_Id_Array;

private

   type Intrigue_Card_Type is
     new Card_Type with
      record
         Card  : Intrigue_Card;
         Count : Positive;
      end record;

   procedure New_Intrigue
     (Card  : in out Intrigue_Card_Type'Class);

end Agrippa.Cards.Intrigue;
