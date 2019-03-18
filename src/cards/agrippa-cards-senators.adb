with Ada.Containers.Vectors;

package body Agrippa.Cards.Senators is

   package Senator_Id_To_Card_Id_Vectors is
     new Ada.Containers.Vectors (Senator_Id, Card_Id);

   Senator_Id_To_Card_Id : Senator_Id_To_Card_Id_Vectors.Vector;

   ------------------
   -- All_Senators --
   ------------------

   function All_Senators return Senator_Id_Array is
      Count : constant Natural := Natural (Senator_Id_To_Card_Id.Length);
      Index : Natural := 0;
      Arr : Senator_Id_Array (1 .. Count);
   begin
      for I in 1 .. Senator_Id_To_Card_Id.Last_Index loop
         Index := Index + 1;
         Arr (Index) := I;
      end loop;
      return Arr;
   end All_Senators;

   -----------------
   -- New_Senator --
   -----------------

   procedure New_Senator
     (Card : in out Senator_Card_Type'Class)
   is
   begin
      New_Card (Card);
      Senator_Id_To_Card_Id.Append (Card.Id);
   end New_Senator;

   -------------
   -- Senator --
   -------------

   function Senator
     (Id : Senator_Id)
      return Senator_Card_Type'Class
   is
      Cid : constant Card_Id :=
              Senator_Id_To_Card_Id (Id);
   begin
      return Senator_Card_Type'Class (Card (Cid));
   end Senator;

end Agrippa.Cards.Senators;
