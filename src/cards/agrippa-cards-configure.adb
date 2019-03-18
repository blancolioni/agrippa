with Tropos.Reader;

with Agrippa.Cards.Concessions.Configure;
with Agrippa.Cards.Intrigue.Configure;
with Agrippa.Cards.Senators.Configure;
with Agrippa.Cards.Wars.Configure;

with Agrippa.Paths;

package body Agrippa.Cards.Configure is

   ---------------------
   -- Configure_Cards --
   ---------------------

   procedure Configure_Cards is
   begin
      Tropos.Reader.Read_Config
        (Path      => Agrippa.Paths.Config_File ("wars"),
         Extension => "txt",
         Configure =>
           Agrippa.Cards.Wars.Configure.Configure_War'Access);
      Agrippa.Cards.Senators.Configure.Configure_Senators
        (Tropos.Reader.Read_Config
           (Agrippa.Paths.Config_File
                ("senators.txt")));
      Agrippa.Cards.Concessions.Configure.Configure_Concessions
        (Tropos.Reader.Read_Config
           (Agrippa.Paths.Config_File
                ("concessions.txt")));
      Agrippa.Cards.Intrigue.Configure.Configure_Intrigue
        (Tropos.Reader.Read_Config
           (Agrippa.Paths.Config_File
                ("intrigue.txt")));
   end Configure_Cards;

end Agrippa.Cards.Configure;
