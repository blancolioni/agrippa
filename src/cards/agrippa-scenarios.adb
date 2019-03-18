with WL.String_Maps;
with WL.String_Sets;

package body Agrippa.Scenarios is

   type Scenario_Record is
      record
         Name     : access constant String;
         Includes : WL.String_Sets.Set;
      end record;

   package Scenario_Maps is
     new WL.String_Maps (Scenario_Record);

   Scenario_Map : Scenario_Maps.Map;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Scenario_Type is
   begin
      if Scenario_Map.Contains (Name) then
         return Scenario_Type (Scenario_Map (Name).Name);
      else
         raise Constraint_Error with
           "no such scenario: " & Name;
      end if;
   end Get;

   ----------------------
   -- Include_Scenario --
   ----------------------

   procedure Include_Scenario
     (Scenario : String;
      Other    : String)
   is
   begin
      Scenario_Map (Scenario).Includes.Insert (Other);
   end Include_Scenario;

   --------------
   -- Includes --
   --------------

   function Includes
     (Scenario : Scenario_Type;
      Other    : Scenario_Type)
      return Boolean
   is
   begin
      return Scenario = Other
        or else Scenario_Map (Scenario.all).Includes.Contains (Other.all);
   end Includes;

   ------------------
   -- New_Scenario --
   ------------------

   procedure New_Scenario (Name : String) is
   begin
      if Scenario_Map.Contains (Name) then
         raise Constraint_Error with
           "scenario already exists: " & Name;
      end if;

      Scenario_Map.Insert
        (Name,
         Scenario_Record'
           (Name     => new String'(Name),
            Includes => <>));
   end New_Scenario;

   ----------
   -- Show --
   ----------

   function Show (Scenario : Scenario_Type) return String is
   begin
      return Scenario.all;
   end Show;

end Agrippa.Scenarios;
