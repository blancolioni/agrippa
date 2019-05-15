with Agrippa.Factions;
with Agrippa.State.Senate;

package body Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts is

   procedure Create_Series
     (Gadget : in out Voting_Chart_Gadget_Type'Class);

   ------------
   -- Create --
   ------------

   procedure Create
     (Gadget  : in out Voting_Chart_Gadget_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      State   : Agrippa.State.Agrippa_State)
   is
   begin
      Gadget.State := State;
      Gadget.Create_Series;
      Gadget.Create_Pie_Chart (Parent, 180, 180);

   end Create;

   -------------------
   -- Create_Series --
   -------------------

   procedure Create_Series
     (Gadget : in out Voting_Chart_Gadget_Type'Class)
   is
   begin
      Gadget.Clear;

      for I in 1 .. Gadget.State.Factions.Count loop
         declare
            Faction : constant Agrippa.Factions.Agrippa_Faction :=
                        Gadget.State.Factions.Get (I);
         begin
            Gadget.Append
              (Faction.Colour,
               Agrippa.State.Senate.Vote_Count
                 (Gadget.State, Faction));
         end;
      end loop;
   end Create_Series;

end Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts;
