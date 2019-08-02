with Agrippa.UI.Gnoga_UI.Phase_Views.Revenue;
with Agrippa.UI.Gnoga_UI.Phase_Views.Senate;

package body Agrippa.UI.Gnoga_UI.Phase_Views is

   ------------
   -- Create --
   ------------

   procedure Create
     (Container : in out Phase_View_Container;
      Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
      State     : Agrippa.State.Agrippa_State)
   is
   begin
      Container.Views.Append (Revenue.Revenue_Phase (State));
      Container.Views.Append (Senate.Senate_Phase (State));
      Gnoga.Gui.Element.Common.DIV_Type (Container).Create (Parent);

      for View of Container.Views loop
         View.Create_Phase_View (Parent);
         View.Hidden (True);
      end loop;

      Container.Current_Index := 1;
      Container.Views.Element (Container.Current_Index).Hidden (False);

   end Create;

   ----------------
   -- Next_Phase --
   ----------------

   procedure Next_Phase
     (Container : in out Phase_View_Container'Class)
   is
   begin
      Container.Views.Element (Container.Current_Index).Hidden (True);
      Container.Current_Index := Container.Current_Index + 1;
      if Container.Current_Index > Container.Views.Last_Index then
         Container.Current_Index := 1;
      end if;
      Container.Views.Element (Container.Current_Index).Activate;
      Container.Views.Element (Container.Current_Index).Hidden (False);
   end Next_Phase;

   -------------
   -- Propose --
   -------------

   procedure Propose
     (View         : in out Agrippa_Phase_View_Type;
      Sponsor      : Agrippa.Senators.Agrippa_Senator;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Voting_Order : Agrippa.Factions.Array_Of_Factions)
   is
   begin
      raise Constraint_Error with
        "view does not allow proposals";
   end Propose;

   ----------
   -- Vote --
   ----------

   procedure Vote
     (View    : in out Agrippa_Phase_View_Type;
      Faction : Agrippa.Factions.Agrippa_Faction;
      Aye     : Boolean)
   is
   begin
      raise Constraint_Error with
        "view does not allow voting";
   end Vote;

   ---------------------
   -- Voting_Complete --
   ---------------------

   procedure Voting_Complete
     (View         : in out Agrippa_Phase_View_Type;
      Proposal     : Agrippa.Proposals.Agrippa_Proposal;
      Ayes, Nays   : Natural)
   is
   begin
      raise Constraint_Error with
        "view does not allow voting to complete";
   end Voting_Complete;

end Agrippa.UI.Gnoga_UI.Phase_Views;
