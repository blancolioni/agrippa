package Agrippa.Scenarios is

   type Scenario_Type is private;

   function Show (Scenario : Scenario_Type) return String;
   function Get (Name : String) return Scenario_Type;

   function Includes
     (Scenario : Scenario_Type;
      Other    : Scenario_Type)
      return Boolean;

private

   type Scenario_Type is access constant String;

   procedure New_Scenario (Name : String);

   procedure Include_Scenario
     (Scenario : String;
      Other    : String);

end Agrippa.Scenarios;
