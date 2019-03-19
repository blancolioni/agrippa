package Agrippa.Players.Autohandler is

   function Create_Autohandler
     (State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id;
      Player  : Autoplayer_Interface'Class)
      return Player_Access;

private

   task type Autohandler_Task is new Player_Handler with

      entry Initialize
        (State   : in out Agrippa.State.State_Interface'Class;
         Faction : Faction_Id);

      entry Stop;

      entry Set_Autoplayer
        (Player : Autoplayer_Interface'Class);

      entry Start_Turn
        (State   : in out Agrippa.State.State_Interface'Class);

      entry Send_Message
        (State   : Agrippa.State.State_Interface'Class;
         Message : Agrippa.Messages.Message_Type);

      entry Get_Reply
        (Reply : out Agrippa.Messages.Message_Type);

      entry What_Do_You_Want_For
        (State   : Agrippa.State.State_Interface'Class;
         Offer   : Agrippa.Deals.Offer_List);

      entry What_Will_You_Give_For
        (State   : Agrippa.State.State_Interface'Class;
         Offer   : Agrippa.Deals.Offer_List);

      entry Get_Offer_Reply
        (Offer  :    out Agrippa.Deals.Offer_List);

      entry Will_You_Agree_To
        (State   : Agrippa.State.State_Interface'Class;
         Deal    : Agrippa.Deals.Deal_Type);

      entry Get_Agreement_Reply
        (Agree  :    out Boolean);

   end Autohandler_Task;

end Agrippa.Players.Autohandler;
