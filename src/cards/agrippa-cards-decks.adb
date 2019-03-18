with WL.Random;

package body Agrippa.Cards.Decks is

   ---------
   -- Add --
   ---------

   procedure Add
     (Deck : in out Deck_Type'Class;
      Card : Card_Type'Class)
   is
   begin
      Deck.Remaining_Cards.Append (Card);
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Deck : in out Deck_Type'Class)
   is
   begin
      Deck.Drawn_Cards.Clear;
      Deck.Remaining_Cards.Clear;
   end Clear;

   ----------
   -- Draw --
   ----------

   function Draw
     (Deck : in out Deck_Type'Class)
      return Card_Type'Class
   is
      Index : constant Positive :=
                WL.Random.Random_Number (1, Deck.Remaining_Cards.Last_Index);
      Card  : constant Card_Type'Class :=
                Deck.Remaining_Cards.Element (Index);
   begin
      Deck.Drawn_Cards.Append (Card);
      if Index < Deck.Remaining_Cards.Last_Index then
         Deck.Remaining_Cards.Replace_Element
           (Index, Deck.Remaining_Cards.Last_Element);
      end if;
      Deck.Remaining_Cards.Delete_Last;
      return Card;
   end Draw;

end Agrippa.Cards.Decks;
