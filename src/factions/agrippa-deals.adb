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
      Terms   : Offer_List)
   is
   begin
      Deal.Terms.Append ((Faction, Terms));
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

end Agrippa.Deals;
