with WL.Localisation;
with WL.Random;

with Agrippa.Cards.Configure;
with Agrippa.Scenarios.Configure;

with Agrippa.UI.Text;

with Agrippa.Paths;

procedure Agrippa.Driver is
   UI : Agrippa.UI.Text.Text_UI_Type;
begin
   if True then
      WL.Random.Reset (2);
   end if;
   WL.Localisation.Read_Language_Directories
     (Agrippa.Paths.Config_File ("localisation"));

   Agrippa.Scenarios.Configure.Configure_Scenarios;
   Agrippa.Cards.Configure.Configure_Cards;
   UI.Start; --   (Scenario => Agrippa.Scenarios.Get ("early-republic"));
end Agrippa.Driver;
