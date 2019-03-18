package body Agrippa.Cards.Concessions.Configure is

   ---------------------------
   -- Configure_Concessions --
   ---------------------------

   procedure Configure_Concessions
     (Concession_Config : Tropos.Configuration)
   is
   begin
      for Config of Concession_Config loop
         declare
            Scenario : constant Agrippa.Scenarios.Scenario_Type :=
                         Agrippa.Scenarios.Get
                           (Config.Get ("scenario", "early-republic"));

            function Get (Name : String) return Talents
            is (Talents (Natural'(Config.Get (Name))));

            function Get (Name : String) return Card_Id_Array;

            ---------
            -- Get --
            ---------

            function Get (Name : String) return Card_Id_Array is
               Item   : constant Tropos.Configuration :=
                          Config.Child (Name);
               Result : Card_Id_Array (1 .. Item.Child_Count);
               Count  : Natural := 0;
            begin
               for I in Result'Range loop
                  if Exists (Item.Get (I)) then
                     Count := Count + 1;
                     Result (Count) := Get (Item.Get (I));
                  end if;
               end loop;
               return Result (1 .. Count);
            end Get;

            Income_Source : constant Concession_Income_Source :=
                              (if Config.Contains ("per-legion-raised")
                               then Per_Legion_Raised
                               elsif Config.Contains ("per-fleet-raised")
                               then Per_Fleet_Raised
                               else Per_Turn);

            Card : Concession_Card_Type :=
                     Concession_Card_Type'
                       (Id        => No_Card,
                        Tag       => new String'(Config.Config_Name),
                        Scenario  => Scenario,
                        Class     => Concession_Card,
                        Keep      => True,
                        Income             => Get ("income"),
                        Source             => Income_Source,
                        Destroyed_By_Card  =>
                          (if Config.Contains ("destroyed-by")
                           then new Card_Id_Array'(Get ("destroyed-by"))
                           else null),
                        Destroyed_By_Disaster =>
                          Config.Child ("destroyed-by")
                        .Contains ("disaster-event"),
                        Destroy_Die_Roll      => Config.Get ("destroy-dr", 1));
         begin
            New_Concession (Card);
         end;
      end loop;
   end Configure_Concessions;

end Agrippa.Cards.Concessions.Configure;
