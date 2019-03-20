private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Agrippa.Proposals is

   type Proposal_Category_Type is
     (No_Proposal,
      Office_Nomination,
      Governor_Nomination, Consul_For_Life,
      Recruitment, Attack);

   subtype Nomination is Proposal_Category_Type range
     Office_Nomination .. Consul_For_Life;

   type Proposal_Category_Array is
     array (Positive range <>) of Proposal_Category_Type;

   type Proposal_Type
     (Category : Proposal_Category_Type := No_Proposal) is private;

   function Nominee (Proposal : Proposal_Type) return Senator_Id
     with Pre => Proposal.Category in Nomination;

   function Office (Proposal : Proposal_Type) return Office_Type
     with Pre => Proposal.Category in Nomination;

   function Commander (Proposal : Proposal_Type) return Senator_Id
     with Pre => Proposal.Category = Attack;

   function Fleets (Proposal : Proposal_Type) return Fleet_Count
     with Pre => Proposal.Category in Recruitment | Attack;

   function Legions (Proposal : Proposal_Type) return Legion_Count
     with Pre => Proposal.Category = Recruitment;

   function Regular_Legions (Proposal : Proposal_Type) return Legion_Count
     with Pre => Proposal.Category = Attack;

   function Veteran_Legions
     (Proposal : Proposal_Type)
      return Legion_Index_Array
     with Pre => Proposal.Category = Attack;

   function War (Proposal : Proposal_Type) return War_Id
     with Pre => Proposal.Category = Attack;

   function Province (Proposal : Proposal_Type) return Province_Id
     with Pre => Proposal.Category = Governor_Nomination;

   function Nominate
     (Senator : Senator_Id;
      Office  : Office_Type)
      return Proposal_Type;

   function Nominate
     (Senator  : Senator_Id;
      Province : Province_Id)
      return Proposal_Type;

   function Nominate_Consul_For_Life
     (Senator  : Senator_Id)
      return Proposal_Type;

   function Recruit
     (Legions : Legion_Count;
      Fleets  : Fleet_Count)
      return Proposal_Type;

   function Attack
     (War             : War_Id;
      Commander       : Senator_Id;
      Regular_Legions : Legion_Count;
      Veteran_Legions : Legion_Index_Array;
      Fleets          : Fleet_Count)
      return Proposal_Type;

   type Proposal_Container_Type is private;

   procedure Clear
     (Container : in out Proposal_Container_Type);

   function Is_Empty
     (Container : Proposal_Container_Type)
      return Boolean;

   procedure Add_Proposal
     (Container : in out Proposal_Container_Type;
      Proposal  : Proposal_Type);

   procedure Scan_Proposals
     (Container : Proposal_Container_Type;
      Process   : not null access
        procedure (Proposal : Proposal_Type));

--     function Find_Proposal
--       (Container : Proposal_Container_Type;
--        Test      : not null access
--          function (Proposal : Proposal_Type) return Boolean)
--        return Proposal_Type;

   function Have_Proposal
     (Container : Proposal_Container_Type;
      Test      : not null access
        function (Proposal : Proposal_Type) return Boolean)
      return Boolean;

   type Proposal_Array is array (Positive range <>) of Proposal_Type;

   function Get_Proposals
     (Container : Proposal_Container_Type)
      return Proposal_Array;

private

   package Legion_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Legion_Index);

   type Proposal_Type (Category : Proposal_Category_Type := No_Proposal) is
      record
         Senator : Senator_Id;
         Fleets  : Fleet_Count := 0;
         Legions : Legion_Count := 0;
         case Category is
            when No_Proposal =>
               null;
            when Office_Nomination =>
               Office  : Office_Type;
            when Governor_Nomination =>
               Province           : Province_Id;
            when Consul_For_Life =>
               null;
            when Recruitment =>
               null;
            when Attack =>
               Veteran_Legions : Legion_Lists.List;
               War             : War_Id;
         end case;
      end record;

   function Nominee (Proposal : Proposal_Type) return Senator_Id
   is (Proposal.Senator);

   function Office (Proposal : Proposal_Type) return Office_Type
   is (Proposal.Office);

   function Commander (Proposal : Proposal_Type) return Senator_Id
   is (Proposal.Senator);

   function Fleets (Proposal : Proposal_Type) return Fleet_Count
   is (Proposal.Fleets);

   function Legions (Proposal : Proposal_Type) return Legion_Count
   is (Proposal.Legions);

   function Regular_Legions (Proposal : Proposal_Type) return Legion_Count
   is (Proposal.Legions);

   function War (Proposal : Proposal_Type) return War_Id
   is (Proposal.War);

   function Province (Proposal : Proposal_Type) return Province_Id
   is (Proposal.Province);

   package Proposal_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Proposal_Type);

   type Proposal_Container_Type is
      record
         List : Proposal_Lists.List;
      end record;

   function Is_Empty
     (Container : Proposal_Container_Type)
      return Boolean
   is (Container.List.Is_Empty);

   function Have_Proposal
     (Container : Proposal_Container_Type;
      Test      : not null access
        function (Proposal : Proposal_Type) return Boolean)
      return Boolean
   is (for some Proposal of Container.List => Test (Proposal));

end Agrippa.Proposals;
