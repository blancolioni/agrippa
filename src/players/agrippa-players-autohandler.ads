package Agrippa.Players.Autohandler is

   function Create_Autohandler
     (State   : Agrippa.State.State_Type;
      Faction : Faction_Id)
      return Player_Access;

   procedure Set_Autoplayer
     (Handler    : Player_Access;
      Autoplayer : Autoplayer_Interface'Class);

private

   task type Autohandler_Task is new Player_Handler with

      entry Initialize
        (State_Access : Agrippa.State.State_Type;
         Faction      : Faction_Id);

      entry Stop;

      entry Set_Autoplayer
        (Player : Autoplayer_Interface'Class);

      entry Start_Turn;

      entry Send_Message
        (Message : Agrippa.Messages.Message_Type);

      entry Get_Reply
        (Reply : out Agrippa.Messages.Message_Type);

      entry Senate_Phase_Desire;

      entry What_Do_You_Want_For
        (Offer   : Agrippa.Deals.Offer_List);

      entry What_Will_You_Give_For
        (Offer   : Agrippa.Deals.Offer_List);

      entry Get_Offer_Reply
        (Offer  :    out Agrippa.Deals.Offer_List);

      entry Will_You_Agree_To
        (Deal    : Agrippa.Deals.Deal_Type);

      entry Get_Agreement_Reply
        (Agree  :    out Boolean);

      entry Vote_Proposal
        (Sponsor  : Senator_Id;
         Proposal : Agrippa.Proposals.Proposal_Container_Type);

      entry Get_Votes
        (Votes    :    out Faction_Vote_Type);

   end Autohandler_Task;

end Agrippa.Players.Autohandler;
