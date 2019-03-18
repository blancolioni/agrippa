with Ada.Containers.Vectors;

package body Agrippa.Cards.Concessions is

   package Concession_Id_To_Card_Id_Vectors is
     new Ada.Containers.Vectors (Concession_Id, Card_Id);

   Concession_Id_To_Card_Id : Concession_Id_To_Card_Id_Vectors.Vector;

   ---------------------
   -- All_Concessions --
   ---------------------

   function All_Concessions
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Concession_Id_Array
   is
      Result : Concession_Id_Array
        (1 .. Natural (Concession_Id_To_Card_Id.Length));
      Count  : Natural := 0;
   begin
      for Id in 1 .. Concession_Id_To_Card_Id.Last_Index loop
         if Agrippa.Scenarios.Includes
           (Scenario,
            Card (Concession_Id_To_Card_Id.Element (Id)).Scenario)
         then
            Count := Count + 1;
            Result (Count) := Id;
         end if;
      end loop;
      return Result (1 .. Count);
   end All_Concessions;

   ----------------
   -- Concession --
   ----------------

   function Concession
     (Id : Concession_Id)
      return Concession_Card_Type'Class
   is
      Cid : constant Card_Id :=
              Concession_Id_To_Card_Id (Id);
   begin
      return Concession_Card_Type'Class (Card (Cid));
   end Concession;

   --------------------
   -- New_Concession --
   --------------------

   procedure New_Concession
     (Card : in out Concession_Card_Type'Class)
   is
   begin
      New_Card (Card);
      Concession_Id_To_Card_Id.Append (Card.Id);
   end New_Concession;

end Agrippa.Cards.Concessions;
