package Agrippa.Cards.Wars is

   type War_Card_Type is
     new Card_Type with private;

   function Active (Card : War_Card_Type'Class) return Boolean;
   function War (Card : War_Card_Type'Class) return War_Id;

   function War (Id : War_Id) return War_Card_Type'Class;

   function Get (Tag : String) return War_Id;

   function All_Wars
     return War_Id_Array;

   function Land_Strength (Card : War_Card_Type'Class) return Combat_Strength;
   function Fleet_Strength (Card : War_Card_Type'Class) return Fleet_Count;
   function Fleet_Support (Card : War_Card_Type'Class) return Fleet_Count;

   function Spoils (Card : War_Card_Type'Class) return Talents;

   function Is_Disaster
     (Card : War_Card_Type'Class;
      Roll : TDR_Range)
      return Boolean;

   function Is_Standoff
     (Card : War_Card_Type'Class;
      Roll : TDR_Range)
      return Boolean;

private

   type Roll_Array is array (Positive range <>) of Positive;
   type Roll_Array_Access is access constant Roll_Array;

   type Attack_Array is array (Positive range <>) of Province_Id;
   type Attack_Array_Access is access constant Attack_Array;

   type War_Card_Type is
     new Card_Type with
      record
         W_Id           : War_Id;
         Land_Strength  : Combat_Strength;
         Fleet_Support  : Fleet_Count;
         Fleet_Strength : Fleet_Count;
         Disaster       : Roll_Array_Access;
         Stand_Off      : Roll_Array_Access;
         Spoils         : Talents;
         Active         : Boolean;
         Attacks        : Attack_Array_Access;
      end record;

   function Active (Card : War_Card_Type'Class) return Boolean
   is (Card.Active);

   function War (Card : War_Card_Type'Class) return War_Id
   is (Card.W_Id);

   function Land_Strength (Card : War_Card_Type'Class) return Combat_Strength
   is (Card.Land_Strength);

   function Fleet_Strength (Card : War_Card_Type'Class) return Fleet_Count
   is (Card.Fleet_Strength);

   function Fleet_Support (Card : War_Card_Type'Class) return Fleet_Count
   is (Card.Fleet_Support);

   procedure New_War
     (Card : in out War_Card_Type'Class);

   function Spoils (Card : War_Card_Type'Class) return Talents
   is (Card.Spoils);

end Agrippa.Cards.Wars;
