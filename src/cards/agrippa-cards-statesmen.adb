with Ada.Containers.Vectors;

package body Agrippa.Cards.Statesmen is

   package Statesman_Id_To_Card_Id_Vectors is
     new Ada.Containers.Vectors (Statesman_Id, Card_Id);

   Statesman_Id_To_Card_Id : Statesman_Id_To_Card_Id_Vectors.Vector;

   ------------------
   -- All_Statesmen --
   ------------------

   function All_Statesmen
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Statesman_Id_Array
   is
      Count : constant Natural := Natural (Statesman_Id_To_Card_Id.Length);
      Index : Natural := 0;
      Arr : Statesman_Id_Array (1 .. Count);
   begin
      for Id in 1 .. Statesman_Id_To_Card_Id.Last_Index loop
         if Agrippa.Scenarios.Includes
           (Scenario,
            Card (Statesman_Id_To_Card_Id.Element (Id)).Scenario)
         then
            Index := Index + 1;
            Arr (Index) := Id;
         end if;
      end loop;
      return Arr;
   end All_Statesmen;

   -----------------
   -- New_Statesman --
   -----------------

   procedure New_Statesman
     (Card : in out Statesman_Card_Type'Class)
   is
   begin
      New_Card (Card);
      Statesman_Id_To_Card_Id.Append (Card.Id);
   end New_Statesman;

   -------------
   -- Statesman --
   -------------

   function Statesman
     (Id : Statesman_Id)
      return Statesman_Card_Type'Class
   is
      Cid : constant Card_Id :=
              Statesman_Id_To_Card_Id (Id);
   begin
      return Statesman_Card_Type'Class (Card (Cid));
   end Statesman;

end Agrippa.Cards.Statesmen;
