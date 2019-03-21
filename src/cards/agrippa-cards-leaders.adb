with Ada.Containers.Vectors;

package body Agrippa.Cards.Leaders is

   package Leader_Id_To_Card_Id_Vectors is
     new Ada.Containers.Vectors (Leader_Id, Card_Id);

   Leader_Id_To_Card_Id : Leader_Id_To_Card_Id_Vectors.Vector;

   ------------------
   -- All_Leaders --
   ------------------

   function All_Leaders
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Leader_Id_Array
   is
      Count : constant Natural := Natural (Leader_Id_To_Card_Id.Length);
      Index : Natural := 0;
      Arr : Leader_Id_Array (1 .. Count);
   begin
      for Id in 1 .. Leader_Id_To_Card_Id.Last_Index loop
         if Agrippa.Scenarios.Includes
           (Scenario,
            Card (Leader_Id_To_Card_Id.Element (Id)).Scenario)
         then
            Index := Index + 1;
            Arr (Index) := Id;
         end if;
      end loop;
      return Arr;
   end All_Leaders;

   -------------
   -- Leader --
   -------------

   function Leader
     (Id : Leader_Id)
      return Leader_Card_Type'Class
   is
      Cid : constant Card_Id :=
              Leader_Id_To_Card_Id (Id);
   begin
      return Leader_Card_Type'Class (Card (Cid));
   end Leader;

   -----------------
   -- New_Leader --
   -----------------

   procedure New_Leader
     (Card : in out Leader_Card_Type'Class)
   is
   begin
      New_Card (Card);
      Leader_Id_To_Card_Id.Append (Card.Id);
   end New_Leader;

end Agrippa.Cards.Leaders;
