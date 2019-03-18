package Agrippa.State.Notifications is

   type Change_Handler_Interface is interface;

   procedure Send_Message
     (Handler : Change_Handler_Interface;
      State   : State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
   is abstract;

   procedure Send_Notification
     (Handler : Change_Handler_Interface;
      Text    : String)
   is abstract;

   procedure On_Faction_Leader_Changed
     (Handler : Change_Handler_Interface;
      State   : State_Interface'Class;
      Faction : Faction_Id)
   is abstract;

   procedure On_Faction_Revenue
     (Handler   : Change_Handler_Interface;
      State     : State_Interface'Class;
      Faction   : Faction_Id;
      Old_Value : Talents;
      Income    : Talents;
      Provinces : Talents;
      New_Value : Talents)
   is abstract;

   procedure On_State_Revenue
     (Handler : Change_Handler_Interface;
      Items   : State_Revenue_Array)
   is abstract;

end Agrippa.State.Notifications;
