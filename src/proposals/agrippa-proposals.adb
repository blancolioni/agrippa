package body Agrippa.Proposals is

   ------------------
   -- Add_Proposal --
   ------------------

   procedure Add_Proposal
     (Container : in out Proposal_Container_Type;
      Proposal  : Proposal_Type)
   is
   begin
      Container.List.Append (Proposal);
   end Add_Proposal;

   ------------
   -- Attack --
   ------------

   function Attack
     (War             : War_Id;
      Commander       : Senator_Id;
      Regular_Legions : Legion_Count;
      Veteran_Legions : Legion_Index_Array;
      Fleets          : Fleet_Count)
      return Proposal_Type
   is
   begin
      return Proposal : Proposal_Type (Attack) do
         Proposal.Senator := Commander;
         Proposal.Fleets := Fleets;
         Proposal.Legions := Regular_Legions;
         Proposal.War := War;
         Proposal.Reinforcement := False;
         for Legion of Veteran_Legions loop
            Proposal.Veteran_Legions.Append (Legion);
         end loop;
      end return;
   end Attack;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Container : in out Proposal_Container_Type)
   is
   begin
      Container.List.Clear;
   end Clear;

   -------------------
   -- Get_Proposals --
   -------------------

   function Get_Proposals
     (Container : Proposal_Container_Type)
      return Proposal_Array
   is
      Count : Natural := 0;
   begin
      return Arr : Proposal_Array (1 .. Natural (Container.List.Length)) do
         for Item of Container.List loop
            Count := Count + 1;
            Arr (Count) := Item;
         end loop;
      end return;
   end Get_Proposals;

   --------------
   -- Nominate --
   --------------

   function Nominate
     (Senator : Senator_Id;
      Office  : Office_Type)
      return Proposal_Type
   is
      Category : constant Proposal_Category_Type := Office_Nomination;
   begin
      return Proposal : Proposal_Type (Category) do
         Proposal.Senator := Senator;
         Proposal.Office  := Office;
      end return;
   end Nominate;

   --------------
   -- Nominate --
   --------------

   function Nominate
     (Senator  : Senator_Id;
      Province : Province_Id)
      return Proposal_Type
   is
   begin
      return Proposal : Proposal_Type (Governor_Nomination) do
         Proposal.Senator := Senator;
         Proposal.Province := Province;
      end return;
   end Nominate;

   ------------------------------
   -- Nominate_Consul_For_Life --
   ------------------------------

   function Nominate_Consul_For_Life
     (Senator  : Senator_Id)
      return Proposal_Type
   is
   begin
      return Proposal : Proposal_Type (Consul_For_Life) do
         Proposal.Senator := Senator;
      end return;
   end Nominate_Consul_For_Life;

   -------------
   -- Recruit --
   -------------

   function Recruit
     (Legions : Legion_Count;
      Fleets  : Fleet_Count)
      return Proposal_Type
   is
   begin
      return Proposal : Proposal_Type (Recruitment) do
         Proposal.Legions := Legions;
         Proposal.Fleets := Fleets;
      end return;
   end Recruit;

   ---------------
   -- Reinforce --
   ---------------

   function Reinforce
     (War             : War_Id;
      Regular_Legions : Legion_Count;
      Veteran_Legions : Legion_Index_Array;
      Fleets          : Fleet_Count)
      return Proposal_Type
   is
   begin
      return Proposal : Proposal_Type (Attack) do
         Proposal.Fleets := Fleets;
         Proposal.Legions := Regular_Legions;
         Proposal.War := War;
         for Legion of Veteran_Legions loop
            Proposal.Veteran_Legions.Append (Legion);
         end loop;
         Proposal.Reinforcement := True;
      end return;
   end Reinforce;

   --------------------
   -- Scan_Proposals --
   --------------------

   procedure Scan_Proposals
     (Container : Proposal_Container_Type;
      Process   : not null access
        procedure (Proposal : Proposal_Type))
   is
   begin
      for Proposal of Container.List loop
         Process (Proposal);
      end loop;
   end Scan_Proposals;

   --------------------------
   -- Scan_Veteran_Legions --
   --------------------------

   function Veteran_Legions
     (Proposal : Proposal_Type)
      return Legion_Index_Array
   is
      Count  : constant Natural := Natural (Proposal.Veteran_Legions.Length);
      Result : Legion_Index_Array (1 .. Count);
      Index  : Natural := 0;
   begin
      for Legion of Proposal.Veteran_Legions loop
         Index := Index + 1;
         Result (Index) := Legion;
      end loop;
      return Result;
   end Veteran_Legions;

end Agrippa.Proposals;
