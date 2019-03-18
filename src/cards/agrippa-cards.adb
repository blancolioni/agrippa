with Ada.Containers.Indefinite_Vectors;
with WL.String_Maps;

package body Agrippa.Cards is

   package Card_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Card_Id, Card_Type'Class);

   package Card_Maps is
     new WL.String_Maps (Real_Card_Id);

   Card_Vector : Card_Vectors.Vector;
   Card_Map : Card_Maps.Map;

   ----------
   -- Card --
   ----------

   function Card (Id : Card_Id) return Card_Type'Class is
   begin
      return Card_Vector.Element (Id);
   end Card;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Card_Map.Contains (Tag);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Card_Id is
   begin
      return Card_Map.Element (Tag);
   end Get;

   --------------
   -- New_Card --
   --------------

   procedure New_Card
     (Card : in out Card_Type'Class)
   is
   begin
      Card.Id := Card_Vector.Last_Index + 1;
      Card_Vector.Append (Card);
      Card_Map.Insert (Card.Tag.all, Card.Id);
   end New_Card;

end Agrippa.Cards;
