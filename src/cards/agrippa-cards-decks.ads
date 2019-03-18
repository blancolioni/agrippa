with Ada.Containers.Indefinite_Vectors;

package Agrippa.Cards.Decks is

   type Deck_Type is tagged private;

   procedure Clear
     (Deck : in out Deck_Type'Class);

   procedure Add
     (Deck : in out Deck_Type'Class;
      Card : Card_Type'Class);

   function Is_Empty (Deck : Deck_Type'Class) return Boolean
     with Post => Is_Empty'Result = (Deck.Remaining = 0);

   function Drawn (Deck : Deck_Type'Class) return Natural;
   function Remaining (Deck : Deck_Type'Class) return Natural;

   function Draw
     (Deck : in out Deck_Type'Class)
      return Card_Type'Class
     with Pre => not Deck.Is_Empty,
     Post => Deck.Remaining = Deck'Old.Remaining - 1
       and then Deck.Drawn = Deck'Old.Drawn + 1;

private

   package Card_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Card_Type'Class);

   type Deck_Type is tagged
      record
         Drawn_Cards     : Card_Vectors.Vector;
         Remaining_Cards : Card_Vectors.Vector;
      end record;

   function Is_Empty (Deck : Deck_Type'Class) return Boolean
   is (Deck.Remaining_Cards.Is_Empty);

   function Drawn (Deck : Deck_Type'Class) return Natural
   is (Deck.Drawn_Cards.Last_Index);

   function Remaining (Deck : Deck_Type'Class) return Natural
   is (Deck.Remaining_Cards.Last_Index);

end Agrippa.Cards.Decks;
