package body Agrippa.Sessions.Messages is

   type Message_Handler is access
     procedure
       (State    : Agrippa.State.State_Interface'Class;
        Message  : Agrippa.Messages.Message_Type;
        Notifier : Gnoga_Notifier_Type'Class);

   procedure Handle_Mortality
     (State    : Agrippa.State.State_Interface'Class;
      Message  : Agrippa.Messages.Message_Type;
      Notifier : Gnoga_Notifier_Type'Class);

   Handlers : constant array (Agrippa.Messages.Message_Content_Type)
     of Message_Handler :=
       (Agrippa.Messages.Mortality_Roll => Handle_Mortality'Access,
        others                          => null);

   --------------------
   -- Handle_Message --
   --------------------

   procedure Handle_Message
     (State    : Agrippa.State.State_Interface'Class;
      Message  : Agrippa.Messages.Message_Type;
      Notifier : Gnoga_Notifier_Type'Class)
   is
   begin
      if Handlers (Message.Content) /= null then
         Handlers (Message.Content) (State, Message, Notifier);
      else
         Notifier.Session.Information
           ("message", Message.Content'Image);
      end if;
   end Handle_Message;

   ----------------------
   -- Handle_Mortality --
   ----------------------

   procedure Handle_Mortality
     (State    : Agrippa.State.State_Interface'Class;
      Message  : Agrippa.Messages.Message_Type;
      Notifier : Gnoga_Notifier_Type'Class)
   is
      use Agrippa.Messages;
   begin
      if Has_Senator (Message) then
         declare
            Senator : constant Senator_Id := Get_Senator (Message);
            Name    : constant String :=
                        State.Senator_Name (Senator);
         begin
            if State.Has_Faction (Senator) then
               declare
                  Faction : constant Faction_Id :=
                              State.Senator_Faction (Senator);
               begin
                  if State.Has_Faction_Leader (Faction)
                    and then State.Faction_Leader (Faction) = Senator
                  then
                     Notifier.Session.Information
                       ("mortality",
                        State.Local_Text
                          ("faction-leader-of-faction-dies",
                           Name, State.Faction_Name (Faction)));
                     if not State.Get_Senator_State (Senator)
                       .Is_Statesman_Only
                     then
                        Notifier.Session.Information
                          ("mortality",
                           State.Local_Text
                             ("succeeded-by-son", Name));
                     end if;
                  else
                     Notifier.Session.Information
                       ("mortality",
                        State.Local_Text
                          ("name-of-faction-dies",
                           Name, State.Faction_Name (Faction)));
                  end if;
               end;
            else
               Notifier.Session.Information
                 ("mortality",
                  State.Local_Text
                    ("unaligned-dies", Name));
            end if;
         end;
      elsif Mortality_Roll_Twice (Message) then
         Notifier.Session.Information
           ("mortality",
            State.Local_Text ("mortality-roll-twice"));
      elsif Mortality_Roll_None (Message) then
         Notifier.Session.Information
           ("mortality",
            State.Local_Text ("mortality-roll-none"));
      else
         raise Constraint_Error with
           "bad mortality roll message";
      end if;

   end Handle_Mortality;

end Agrippa.Sessions.Messages;
