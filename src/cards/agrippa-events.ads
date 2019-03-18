package Agrippa.Events is

   type Event_Type is
     (Allied_Enthusiasm,
      Ally_Deserts,
      Barbarian_Raids,
      Drought,
      Enemy_Ally_Deserts,
      Enemy_Leader_Dies,
      Epidemic,
      Evil_Omens,
      Internal_Disorder,
      Manpower_Shortage,
      Mob_Violence,
      Natural_Disaster,
      New_Alliance,
      No_Recruitment,
      People_Revolt,
      Pretender,
      Refuge,
      Rhodian_Alliance,
      Storm_At_Sea,
      Trial_Of_Verres
     );

   function Timed (Event : Event_Type) return Boolean;
   function Instant (Event : Event_Type) return Boolean;
   function Indefinite (Event : Event_Type) return Boolean;
   function Turns (Event : Event_Type) return Natural;

private

   function Indefinite (Event : Event_Type) return Boolean
   is (Event = Refuge);

   function Instant (Event : Event_Type) return Boolean
   is (Event = Storm_At_Sea or else Event = Mob_Violence
       or else Event = People_Revolt);

   function Timed (Event : Event_Type) return Boolean
   is (not Indefinite (Event) and then not Instant (Event));

   function Turns (Event : Event_Type) return Natural
   is (if Event = Drought then 2 else 1);

end Agrippa.Events;
