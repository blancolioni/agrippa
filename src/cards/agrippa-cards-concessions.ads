package Agrippa.Cards.Concessions is

   type Concession_Income_Source is
     (Per_Turn, Per_Legion_Raised, Per_Fleet_Raised);

   type Concession_Card_Type is
     new Card_Type with private;

   function Concession (Id : Concession_Id) return Concession_Card_Type'Class;
   function Concession
     (Card : Concession_Card_Type'Class)
      return Concession_Id;

   function All_Concessions
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Concession_Id_Array;

private

   type Concession_Card_Type is
     new Card_Type with
      record
         C_Id                  : Concession_Id;
         Income                : Talents;
         Source                : Concession_Income_Source;
         Destroyed_By_Card     : access constant Card_Id_Array;
         Destroyed_By_Disaster : Boolean;
         Destroy_Die_Roll      : Positive;
      end record;

   function Concession
     (Card : Concession_Card_Type'Class)
      return Concession_Id
   is (Card.C_Id);

   procedure New_Concession
     (Card : in out Concession_Card_Type'Class);

end Agrippa.Cards.Concessions;
