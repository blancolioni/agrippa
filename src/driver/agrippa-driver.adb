with Ada.Directories;

with WL.Command_Line;
with WL.Localisation;
with WL.Random;

with Agrippa.Cards.Configure;
with Agrippa.Scenarios.Configure;

with Agrippa.UI.Text;

with Agrippa.Options;
with Agrippa.Paths;

procedure Agrippa.Driver is
   UI : Agrippa.UI.Text.Text_UI_Type;
begin

   if not Ada.Directories.Exists ("options.txt") then
      Ada.Directories.Copy_File
        (Source_Name => Agrippa.Paths.Config_File ("default-options.txt"),
         Target_Name => "options.txt");
   end if;

   WL.Command_Line.Load_Defaults ("options.txt");

   if Agrippa.Options.Randomise then
      WL.Random.Randomise;
   else
      WL.Random.Reset (Agrippa.Options.Random_Seed);
   end if;

   WL.Localisation.Read_Language_Directories
     (Agrippa.Paths.Config_File ("localisation"));

   Agrippa.Scenarios.Configure.Configure_Scenarios;
   Agrippa.Cards.Configure.Configure_Cards;
   UI.Start; --   (Scenario => Agrippa.Scenarios.Get ("early-republic"));
end Agrippa.Driver;
