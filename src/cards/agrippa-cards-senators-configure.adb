package body Agrippa.Cards.Senators.Configure is

   ------------------------
   -- Configure_Senators --
   ------------------------

   procedure Configure_Senators
     (Senator_Config : Tropos.Configuration)
   is
   begin
      for Config of Senator_Config loop
         declare
            Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                         Agrippa.Scenarios.Get
                           (Config.Get ("scenario", "early-republic"));

            function Get (Name : String) return Attribute_Range
            is (Attribute_Range (Positive'(Config.Get (Name))));

            Card : Senator_Card_Type :=
                     Senator_Card_Type'
                       (Id        => No_Card,
                        S_Id      => Senator_Id'First,
                        Tag       => new String'(Config.Config_Name),
                        Scenario  => Scenario,
                        Class     => Senator_Card,
                        Keep      => False,
                        Military  => Get ("mil"),
                        Oratory   => Get ("ora"),
                        Loyalty   => Get ("loy"),
                        Influence => Get ("infl"));
         begin
            New_Senator (Card);
         end;
      end loop;
   end Configure_Senators;

end Agrippa.Cards.Senators.Configure;
