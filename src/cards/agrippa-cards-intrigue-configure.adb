with WL.String_Maps;

package body Agrippa.Cards.Intrigue.Configure is

   ------------------------
   -- Configure_Intrigue --
   ------------------------

   procedure Configure_Intrigue
     (Intrigue_Config : Tropos.Configuration)
   is
      package Config_Card_Maps is
        new WL.String_Maps (Intrigue_Card);

      Card_Map : Config_Card_Maps.Map;

   begin
      Card_Map.Insert ("tribune", Tribune);
      Card_Map.Insert ("blackmail", Blackmail);
      Card_Map.Insert ("influence-peddling", Influence_Peddling);
      Card_Map.Insert ("seduction", Seduction);
      Card_Map.Insert ("assassin", Assassin);
      Card_Map.Insert ("secret-bodyguard", Secret_Bodyguard);

      for Config of Intrigue_Config loop
         declare
            Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                         Agrippa.Scenarios.Get
                           (Config.Get ("scenario", "early-republic"));
            Count    : constant Positive :=
                         Config.Get ("count", 1);
            Name     : constant String := Config.Config_Name;
            Card     : Intrigue_Card_Type :=
                         Intrigue_Card_Type'
                           (Id  => No_Card,
                            Tag => new String'(Name),
                            Scenario  => Scenario,
                            Class     => Agrippa.Cards.Intrigue_Card,
                            Keep      => True,
                            Card      =>
                              Card_Map.Element (Config.Config_Name),
                            Count     => Count);
         begin
            New_Intrigue (Card);
         end;
      end loop;
   end Configure_Intrigue;

end Agrippa.Cards.Intrigue.Configure;
