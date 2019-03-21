with Agrippa.Cards.Wars;

package body Agrippa.Cards.Leaders.Configure is

   -----------------------
   -- Configure_Leaders --
   -----------------------

   procedure Configure_Leaders
     (Leader_Config : Tropos.Configuration)
   is
   begin
      for Config of Leader_Config loop
         declare
            Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                         Agrippa.Scenarios.Get
                           (Config.Get ("scenario", "early-republic"));

            function Get (Name : String) return Natural
            is (Config.Get (Name));

            function Get_Matching_Wars return War_Id_Array;

            -----------------------
            -- Get_Matching_Wars --
            -----------------------

            function Get_Matching_Wars return War_Id_Array is
               List : Tropos.Configuration :=
                        Config.Child ("matching-wars");
               Index : Natural := 0;
            begin
               return Wars : War_Id_Array (1 .. List.Child_Count) do
                  for Child of List loop
                     Index := Index + 1;
                     Wars (Index) :=
                       Agrippa.Cards.Wars.Get (Child.Config_Name);
                  end loop;
               end return;
            end Get_Matching_Wars;

            Card : Leader_Card_Type :=
                     Leader_Card_Type'
                       (Id        => No_Card,
                        L_Id      => Leader_Id'First,
                        Tag       => new String'(Config.Config_Name),
                        Scenario  => Scenario,
                        Class     => Leader_Card,
                        Keep      => False,
                        Strength  => Get ("strength"),
                        Disaster  => Get ("disaster"),
                        Stand_Off => Get ("stand-off"),
                        Matching_Wars =>
                          new War_Id_Array'(Get_Matching_Wars));
         begin
            New_Leader (Card);
         end;
      end loop;
   end Configure_Leaders;

end Agrippa.Cards.Leaders.Configure;
