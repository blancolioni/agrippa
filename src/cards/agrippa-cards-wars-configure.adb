package body Agrippa.Cards.Wars.Configure is

   procedure Configure_War
     (Config : Tropos.Configuration)
   is
      Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                   Agrippa.Scenarios.Get
                     (Config.Get ("scenario", "early-republic"));

      function Numbers
        (Field_Name : String)
               return Roll_Array;

      function Get
        (Name : String)
         return Military_Unit_Count
      is (Military_Unit_Count (Natural'(Config.Get (Name, 0))));

      -------------
      -- Numbers --
      -------------

      function Numbers
        (Field_Name : String)
               return Roll_Array
      is
         Item  : constant Tropos.Configuration :=
                   Config.Child (Field_Name);
         Count : constant Natural := Item.Child_Count;
      begin
         return Arr   : Roll_Array (1 .. Count) do
            for I in Arr'Range loop
               Arr (I) := Item.Get (I);
            end loop;
         end return;
      end Numbers;

      Card     : War_Card_Type :=
                   War_Card_Type'
                     (Id             => No_Card,
                      W_Id           => War_Id'First,
                      Tag            => new String'(Config.Config_Name),
                      Scenario       => Scenario,
                      Class          => War_Card,
                      Keep           => False,
                      Land_Strength  => Get ("land-strength"),
                      Fleet_Support  => Get ("fleet-support"),
                      Fleet_Strength => Get ("fleet-strength"),
                      Disaster       =>
                         new Roll_Array'(Numbers ("disaster")),
                      Stand_Off      =>
                         new Roll_Array'(Numbers ("standoff")),
                      Spoils         =>
                        Talents (Integer'(Config.Get ("spoils", 0))),
                      Active         => Config.Get ("active"),
                      Attacks        => null);
   begin
      New_War (Card);
   end Configure_War;

end Agrippa.Cards.Wars.Configure;
