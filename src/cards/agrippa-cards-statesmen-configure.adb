with Agrippa.Cards.Senators;
with Agrippa.Cards.Wars;

package body Agrippa.Cards.Statesmen.Configure is

   ------------------------
   -- Configure_Statesmen --
   ------------------------

   procedure Configure_Statesmen
     (Statesman_Config : Tropos.Configuration)
   is
   begin
      for Config of Statesman_Config loop
         declare
            Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                         Agrippa.Scenarios.Get
                           (Config.Get ("scenario", "early-republic"));

            Special  : constant Tropos.Configuration :=
                         Config.Child ("special");

            function Get (Name : String) return Attribute_Range
            is (Attribute_Range (Positive'(Config.Get (Name))));

            function Get_Voids_DS return War_Id_Array;

            ------------------
            -- Get_Voids_DS --
            ------------------

            function Get_Voids_DS return War_Id_Array is
               Voids_DS : constant Tropos.Configuration :=
                            Special.Child ("voids-ds");
            begin
               return Result   : War_Id_Array (1 .. Voids_DS.Child_Count) do
                  for I in Result'Range loop
                     Result (I) :=
                       Agrippa.Cards.Wars.War_Card_Type
                         (Agrippa.Cards.Card
                            (Agrippa.Cards.Get
                               (Voids_DS.Child (I).Config_Name)))
                         .War;
                  end loop;
               end return;
            end Get_Voids_DS;

            Card : Statesman_Card_Type :=
                     Statesman_Card_Type'
                       (Id        => No_Card,
                        S_Id => Statesman_Id'First,
                        Tag       => new String'(Config.Config_Name),
                        Scenario  => Scenario,
                        Class     => Statesman_Card,
                        Keep      => True,
                        Family    =>
                          Agrippa.Cards.Senators.Senator_Card_Type
                            (Agrippa.Cards.Card
                                 (Agrippa.Cards.Get
                                      (Config.Get ("family"))))
                        .Senator,
                        Military  => Get ("mil"),
                        Oratory   => Get ("ora"),
                        Loyalty   => Get ("loy"),
                        Influence => Get ("infl"),
                        Voids_DS  => new War_Id_Array'(Get_Voids_DS));
         begin
            New_Statesman (Card);
         end;
      end loop;
   end Configure_Statesmen;

end Agrippa.Cards.Statesmen.Configure;
