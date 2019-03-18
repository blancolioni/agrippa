package body Agrippa.Scenarios.Configure is

   -------------------------
   -- Configure_Scenarios --
   -------------------------

   procedure Configure_Scenarios is
   begin
      New_Scenario ("early-republic");
      New_Scenario ("middle-republic");
      New_Scenario ("late-republic");
      Include_Scenario ("middle-republic", "early-republic");
      Include_Scenario ("late-republic", "middle-republic");
      Include_Scenario ("late-republic", "early-republic");
   end Configure_Scenarios;

end Agrippa.Scenarios.Configure;
