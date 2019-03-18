with Ada.Containers.Vectors;

package body Agrippa.Cards.Intrigue is

   package Intrigue_Id_To_Card_Id_Vectors is
     new Ada.Containers.Vectors (Intrigue_Id, Card_Id);

   Intrigue_Id_To_Card_Id : Intrigue_Id_To_Card_Id_Vectors.Vector;
   Total_Intrigue_Count   : Natural := 0;

   ---------------------
   -- All_Intrigues --
   ---------------------

   function All_Intrigues
     (Scenario : Agrippa.Scenarios.Scenario_Type)
      return Intrigue_Id_Array
   is
      Result : Intrigue_Id_Array (1 .. Total_Intrigue_Count);
      Count  : Natural := 0;
   begin
      for Id in 1 .. Intrigue_Id_To_Card_Id.Last_Index loop
         declare
            C : constant Intrigue_Card_Type'Class :=
                  Intrigue_Card_Type'Class
                    (Card (Intrigue_Id_To_Card_Id.Element (Id)));
         begin
            if Agrippa.Scenarios.Includes (Scenario, C.Scenario) then
               for I in 1 .. C.Count loop
                  Count := Count + 1;
                  Result (Count) := Id;
               end loop;
            end if;
         end;
      end loop;
      return Result (1 .. Count);
   end All_Intrigues;

   ----------------
   -- Intrigue --
   ----------------

   function Intrigue
     (Id : Intrigue_Id)
      return Intrigue_Card_Type'Class
   is
      Cid : constant Card_Id :=
              Intrigue_Id_To_Card_Id (Id);
   begin
      return Intrigue_Card_Type'Class (Card (Cid));
   end Intrigue;

   --------------------
   -- New_Intrigue --
   --------------------

   procedure New_Intrigue
     (Card  : in out Intrigue_Card_Type'Class)
   is
   begin
      New_Card (Card);
      Intrigue_Id_To_Card_Id.Append (Card.Id);
      Total_Intrigue_Count := Total_Intrigue_Count + Card.Count;
   end New_Intrigue;

end Agrippa.Cards.Intrigue;
