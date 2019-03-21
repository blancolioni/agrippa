with Ada.Containers.Vectors;

package body Agrippa.Cards.Wars is

   package War_Id_To_Card_Id_Vectors is
     new Ada.Containers.Vectors (War_Id, Card_Id);

   War_Id_To_Card_Id : War_Id_To_Card_Id_Vectors.Vector;

   --------------
   -- All_Wars --
   --------------

   function All_Wars
      return War_Id_Array
   is
      Result : War_Id_Array (1 .. Natural (War_Id_To_Card_Id.Length));
      Count  : Natural := 0;
   begin
      for Id in 1 .. War_Id_To_Card_Id.Last_Index loop
         Count := Count + 1;
         Result (Count) := Id;
      end loop;
      return Result (1 .. Count);
   end All_Wars;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return War_Id is
   begin
      for Id in 1 .. War_Id_To_Card_Id.Last_Index loop
         if Card (War_Id_To_Card_Id.Element (Id)).Tag.all = Tag then
            return Id;
         end if;
      end loop;
      raise Constraint_Error with
        "no such war: " & Tag;
   end Get;

   -------------
   -- New_War --
   -------------

   procedure New_War
     (Card : in out War_Card_Type'Class)
   is
   begin
      Card.W_Id := War_Id_To_Card_Id.Last_Index + 1;
      New_Card (Card);
      War_Id_To_Card_Id.Append (Card.Id);
   end New_War;

   ---------
   -- War --
   ---------

   function War
     (Id : War_Id)
      return War_Card_Type'Class
   is
      Cid : constant Card_Id :=
              War_Id_To_Card_Id (Id);
   begin
      return War_Card_Type'Class (Card (Cid));
   end War;

end Agrippa.Cards.Wars;
