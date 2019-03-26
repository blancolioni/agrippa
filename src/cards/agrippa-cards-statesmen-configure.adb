with Agrippa.Cards.Senators;

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

            function Get (Name : String) return Attribute_Range
            is (Attribute_Range (Positive'(Config.Get (Name))));

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
                        Influence => Get ("infl"));
         begin
            New_Statesman (Card);
         end;
      end loop;
   end Configure_Statesmen;

end Agrippa.Cards.Statesmen.Configure;
