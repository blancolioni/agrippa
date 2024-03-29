private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Agrippa.Deals is

   type Offer_Category_Type is
     (Nothing, Office, Province, Card);

   type Offer_Type (Category : Offer_Category_Type := Nothing) is private;

   type Offer_Array is array (Positive range <>) of Offer_Type;

   function Nothing return Offer_Type;

   function Office
     (Item   : Office_Type;
      Holder : Senator_Id)
      return Offer_Type;

   function Province
      return Offer_Type;

   function Concession
     return Offer_Type;

   function Is_Office_Offer (Offer : Offer_Type) return Boolean;
   function Get_Office (Offer : Offer_Type) return Office_Type
     with Pre => Is_Office_Offer (Offer);

   function Is_Province_Offer (Offer : Offer_Type) return Boolean;
   function Get_Province (Offer : Offer_Type) return Province_Id
     with Pre => Is_Province_Offer (Offer);

   function Get_Holder (Offer : Offer_Type) return Senator_Id
     with Pre => Is_Office_Offer (Offer);

   function Show
     (Offer        : Offer_Type;
      Show_Senator : Show_Senator_Interface'Class)
      return String;

   type Offer_List is private;

   procedure Clear (List : in out Offer_List);
   function Is_Empty (List : Offer_List) return Boolean;
   procedure Add (List : in out Offer_List;
                  Offer : Offer_Type);

   function Get_Offers (List : Offer_List) return Offer_Array;

   procedure Scan (List : Offer_List;
                   Process : not null access
                     procedure (Offer : Offer_Type));

   function Show
     (Offer : Offer_List;
      Show_Senator : Show_Senator_Interface'Class)
      return String;

   function Matching_Index
     (List  : Offer_List;
      Match : Offer_Type)
      return Natural;

   type Deal_Type is private;

   function Is_Empty
     (Deal : Deal_Type)
      return Boolean;

   procedure Clear
     (Deal    : in out Deal_Type);

   procedure Add
     (Deal    : in out Deal_Type;
      Faction : Faction_Id;
      Offer   : Offer_Type);

   procedure Scan
     (Deal : Deal_Type;
      Process : not null access
        procedure (Faction : Faction_Id;
                   Terms   : Offer_List));

   procedure Scan
     (Deal    : Deal_Type;
      Process : not null access
        procedure (Faction : Faction_Id;
                   Offer   : Offer_Type));

   function Show
     (Deal : Deal_Type;
      Show_Senator : Show_Senator_Interface'Class)
      return String;

   function Contradicts
     (Deal    : Deal_Type;
      Office  : Office_Type;
      Holder  : Senator_Id;
      Faction : Faction_Id)
      return Boolean;

private

   type Offer_Type (Category : Offer_Category_Type := Nothing) is
      record
         Any : Boolean := False;
         case Category is
            when Nothing =>
               null;
            when Office =>
               Offer_Office   : Office_Type;
               Holder         : Senator_Id;
            when Province =>
               Offer_Province : Province_Id;
               Governor       : Senator_Id;
            when Card =>
               Any_Concession : Boolean := False;
               Offer_Card     : Card_Id;
         end case;
      end record;

   function Nothing return Offer_Type
   is (Nothing, False);

   function Office
     (Item   : Office_Type;
      Holder : Senator_Id)
      return Offer_Type
   is (Office, False, Item, Holder);

   function Province
     return Offer_Type
   is (Province, True, 1, 1);

   function Concession
     return Offer_Type
   is (Card, False, True, No_Card);

   function Is_Office_Offer (Offer : Offer_Type) return Boolean
   is (Offer.Category = Office);

   function Get_Office (Offer : Offer_Type) return Office_Type
   is (Offer.Offer_Office);

   function Get_Holder (Offer : Offer_Type) return Senator_Id
   is (Offer.Holder);

   function Is_Province_Offer (Offer : Offer_Type) return Boolean
   is (Offer.Category = Province);

   function Get_Province (Offer : Offer_Type) return Province_Id
   is (Offer.Offer_Province);

   package Offer_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Offer_Type);

   type Offer_List is
      record
         List : Offer_Lists.List;
      end record;

   function Is_Empty (List : Offer_List) return Boolean
   is (List.List.Is_Empty);

   type Query_Type is null record;

   type Deal_Term_Type is
      record
         Faction : Faction_Id;
         Terms   : Offer_List;
      end record;

   package Term_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Deal_Term_Type);

   type Deal_Type is
      record
         Terms   : Term_Lists.List;
      end record;

   function Is_Empty
     (Deal : Deal_Type)
      return Boolean
   is (Deal.Terms.Is_Empty);

end Agrippa.Deals;
