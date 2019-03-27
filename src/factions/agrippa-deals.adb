with Ada.Strings.Unbounded;

with Agrippa.Cards;

package body Agrippa.Deals is

   ---------
   -- Add --
   ---------

   procedure Add
     (List  : in out Offer_List;
      Offer : Offer_Type)
   is
   begin
      List.List.Append (Offer);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Deal    : in out Deal_Type;
      Faction : Faction_Id;
      Offer   : Offer_Type)
   is
   begin
      for Term of Deal.Terms loop
         if Term.Faction = Faction then
            Term.Terms.List.Append (Offer);
            return;
         end if;
      end loop;

      declare
         List : Offer_List;
      begin
         List.List.Append (Offer);
         Deal.Terms.Append ((Faction, List));
      end;

   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Offer_List) is
   begin
      List.List.Clear;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Deal    : in out Deal_Type)
   is
   begin
      Deal.Terms.Clear;
   end Clear;

   -----------------
   -- Contradicts --
   -----------------

   function Contradicts
     (Deal    : Deal_Type;
      Office  : Office_Type;
      Holder  : Senator_Id;
      Faction : Faction_Id)
      return Boolean
   is
   begin
      if Is_Empty (Deal) then
         return True;
      end if;
      for Terms of Deal.Terms loop
         if Terms.Faction = Faction then
            for Offer of Terms.Terms.List loop
               if Offer.Category = Agrippa.Deals.Office
                 and then Offer.Offer_Office = Office
               then
                  declare
                     Result : constant Boolean :=
                                Offer.Holder /= Holder;
                  begin
                     if Result then
                        return Result;
                     end if;
                  end;
               end if;
            end loop;
         end if;
      end loop;
      return False;
   end Contradicts;

   ----------------
   -- Get_Offers --
   ----------------

   function Get_Offers (List : Offer_List) return Offer_Array is
      Result : Offer_Array (1 .. Natural (List.List.Length));
      Index  : Natural := 0;
   begin
      for Offer of List.List loop
         Index := Index + 1;
         Result (Index) := Offer;
      end loop;
      return Result;
   end Get_Offers;

   --------------------
   -- Matching_Index --
   --------------------

   function Matching_Index
     (List  : Offer_List;
      Match : Offer_Type)
      return Natural
   is
      use type Agrippa.Cards.Card_Class;
      Index : Natural := 0;
   begin
      for Offer of List.List loop
         Index := Index + 1;
         if Match.Category = Offer.Category then
            if Match.Any or else Offer.Any then
               return Index;
            elsif Match = Offer then
               return Index;
            else
               case Match.Category is
                  when Card =>
                     if Match.Any_Concession then
                        if Offer.Any_Concession
                          or else Agrippa.Cards.Card (Offer.Offer_Card)
                          .Class = Agrippa.Cards.Concession_Card
                        then
                           return Index;
                        elsif Offer.Any_Concession then
                           if Agrippa.Cards.Card (Match.Offer_Card)
                             .Class = Agrippa.Cards.Concession_Card
                           then
                              return Index;
                           end if;
                        end if;
                     end if;
                  when others =>
                     null;
               end case;
            end if;
         end if;
      end loop;

      return 0;

   end Matching_Index;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (List : Offer_List;
      Process : not null access
                     procedure (Offer : Offer_Type))
   is
   begin
      for Offer of List.List loop
         Process (Offer);
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Deal : Deal_Type;
      Process : not null access
        procedure (Faction : Faction_Id;
                   Terms   : Offer_List))
   is
   begin
      for Term of Deal.Terms loop
         Process (Term.Faction, Term.Terms);
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Deal    : Deal_Type;
      Process : not null access
        procedure (Faction : Faction_Id;
                   Offer   : Offer_Type))
   is
   begin
      for Term of Deal.Terms loop
         for Offer of Term.Terms.List loop
            Process (Term.Faction, Offer);
         end loop;
      end loop;
   end Scan;

   ----------
   -- Show --
   ----------

   function Show
     (Offer        : Offer_Type;
      Show_Senator : Show_Senator_Interface'Class)
      return String
   is
   begin
      case Offer.Category is
         when Nothing =>
            return "no offer";
         when Office =>
            if Offer.Any then
               return "any office";
            else
               return Show_Senator.Senator_Name_And_Faction (Offer.Holder)
                 & " as " & Offer.Offer_Office'Image;
            end if;
         when Province =>
            if Offer.Any then
               return "any province";
            else
               return Offer.Offer_Province'Image;
            end if;
         when Card =>
            if Offer.Any then
               return "any card";
            elsif Offer.Any_Concession then
               return "concession";
            else
               return Agrippa.Cards.Card (Offer.Offer_Card).Tag;
            end if;
      end case;
   end Show;

   ----------
   -- Show --
   ----------

   function Show
     (Offer        : Offer_List;
      Show_Senator : Show_Senator_Interface'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Item of Offer.List loop
         if Result /= "" then
            Result := Result & ", ";
         end if;
         Result := Result & Show (Item, Show_Senator);
      end loop;
      return To_String (Result);
   end Show;

   ----------
   -- Show --
   ----------

   function Show
     (Deal         : Deal_Type;
      Show_Senator : Show_Senator_Interface'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Term of Deal.Terms loop
         if not Term.Terms.List.Is_Empty then
            if Result /= "" then
               Result := Result & "; ";
            end if;
            Result := Result & Show_Senator.Faction_Name (Term.Faction) & ": "
              & Show (Term.Terms, Show_Senator);
         end if;
      end loop;
      return To_String (Result);
   end Show;

end Agrippa.Deals;
