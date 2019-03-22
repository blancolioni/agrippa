with Agrippa.Phases.Mortality;
with Agrippa.Phases.Revenue;
with Agrippa.Phases.Forum;
with Agrippa.Phases.Population;
with Agrippa.Phases.Senate;
with Agrippa.Phases.Combat;
with Agrippa.Phases.Revolution;

package body Agrippa.Phases.Sequence is

   -----------
   -- Phase --
   -----------

   function Phase (Id : Phase_Id) return Phase_Interface'Class is
   begin
      case Id is
         when 1 =>
            return Agrippa.Phases.Mortality.Mortality_Phase;
         when 2 =>
            return Agrippa.Phases.Revenue.Revenue_Phase;
         when 3 =>
            return Agrippa.Phases.Forum.Forum_Phase;
         when 4 =>
            return Agrippa.Phases.Population.Population_Phase;
         when 5 =>
            return Agrippa.Phases.Senate.Senate_Phase;
         when 6 =>
            return Agrippa.Phases.Combat.Combat_Phase;
         when 7 =>
            return Agrippa.Phases.Revolution.Revolution_Phase;
      end case;
   end Phase;

end Agrippa.Phases.Sequence;
