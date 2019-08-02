with Agrippa.Messages;

private package Agrippa.Sessions.Messages is

   procedure Handle_Message
     (State    : Agrippa.State.State_Interface'Class;
      Message  : Agrippa.Messages.Message_Type;
      Notifier : Gnoga_Notifier_Type'Class);

end Agrippa.Sessions.Messages;
