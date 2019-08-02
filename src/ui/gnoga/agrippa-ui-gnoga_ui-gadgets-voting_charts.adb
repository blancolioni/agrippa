package body Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts is

   procedure Create_Series
     (Gadget : in out Voting_Chart_Gadget_Type'Class);

   ------------
   -- Create --
   ------------

   procedure Create
     (Gadget  : in out Voting_Chart_Gadget_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      State   : not null access Agrippa.State.State_Interface'Class)
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

      for Faction in Faction_Id loop
         Gadget.Append
           (Gadget.State.Get_Faction_State (Faction).Color,
            Natural
              (Gadget.State.Faction_Votes (Faction)));
      end loop;
   end Create_Series;

end Agrippa.UI.Gnoga_UI.Gadgets.Voting_Charts;
