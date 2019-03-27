with Ada.Containers.Indefinite_Holders;

package body Agrippa.Players.Autohandler is

   type Autohandler_Task_Access is access all Autohandler_Task;

   package Autoplayer_Holders is
     new Ada.Containers.Indefinite_Holders
       (Autoplayer_Interface'Class);

   ----------------------
   -- Autohandler_Task --
   ----------------------

   task body Autohandler_Task is
      My_Faction       : Faction_Id;
      My_Player        : Autoplayer_Holders.Holder;
      Agreement        : Boolean;
      Counter          : Agrippa.Deals.Offer_List;
      Msg              : Agrippa.Messages.Message_Type;
      Current_Votes    : Faction_Vote_Type;
      pragma Unreferenced (My_Faction);
   begin
      accept Initialize
        (State : in out Agrippa.State.State_Interface'Class;
         Faction : in Faction_Id)
      do
         pragma Unreferenced (State);
         My_Faction := Faction;
      end Initialize;

      accept Set_Autoplayer (Player : in Autoplayer_Interface'Class) do
         My_Player := Autoplayer_Holders.To_Holder (Player);
      end Set_Autoplayer;

      loop
         select
            accept Start_Turn
              (State : in out Agrippa.State.State_Interface'Class)
            do
               My_Player.Element.Start_Turn (State);
            end Start_Turn;
         or
            accept Send_Message
              (State : in Agrippa.State.State_Interface'Class;
               Message : in Agrippa.Messages.Message_Type)
            do
               Msg := My_Player.Reference.Send_Message (State, Message);
            end Send_Message;
            accept Get_Reply (Reply : out Agrippa.Messages.Message_Type) do
               Reply := Msg;
            end Get_Reply;
         or
            accept Will_You_Agree_To
              (State : in Agrippa.State.State_Interface'Class;
               Deal : in Agrippa.Deals.Deal_Type)
            do
               Agreement :=
                 My_Player.Reference.Will_You_Agree_To (State, Deal);
            end Will_You_Agree_To;
            accept Get_Agreement_Reply (Agree : out Boolean) do
               Agree := Agreement;
            end Get_Agreement_Reply;
         or
            accept Senate_Phase_Desire
              (State : in Agrippa.State.State_Interface'Class)
            do
               Counter := My_Player.Element.Senate_Phase_Desire (State);
            end Senate_Phase_Desire;
            accept Get_Offer_Reply (Offer : out Agrippa.Deals.Offer_List) do
               Offer := Counter;
            end Get_Offer_Reply;
         or
            accept What_Do_You_Want_For
              (State : in Agrippa.State.State_Interface'Class;
               Offer : in Agrippa.Deals.Offer_List)
            do
               Counter :=
                 My_Player.Element.What_Do_You_Want_For
                   (State, Offer);
            end What_Do_You_Want_For;
            accept Get_Offer_Reply (Offer : out Agrippa.Deals.Offer_List) do
               Offer := Counter;
            end Get_Offer_Reply;
         or
            accept What_Will_You_Give_For
              (State : in Agrippa.State.State_Interface'Class;
               Offer : in Agrippa.Deals.Offer_List)
            do
               Counter :=
                 My_Player.Element.What_Will_You_Give_For
                   (State, Offer);
            end What_Will_You_Give_For;
            accept Get_Offer_Reply (Offer : out Agrippa.Deals.Offer_List) do
               Offer := Counter;
            end Get_Offer_Reply;
         or
            accept Vote_Proposal
              (State    : Agrippa.State.State_Interface'Class;
               Sponsor  : Senator_Id;
               Proposal : in Agrippa.Proposals.Proposal_Container_Type)
            do
               Current_Votes :=
                 My_Player.Element.Vote (State, Sponsor, Proposal);
            end Vote_Proposal;
            accept Get_Votes (Votes : out Faction_Vote_Type) do
               Votes := Current_Votes;
            end Get_Votes;
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;
      end loop;
   end Autohandler_Task;

   ------------------------
   -- Create_Autohandler --
   ------------------------

   function Create_Autohandler
     (State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
      return Player_Access
   is
      Result : constant Autohandler_Task_Access := new Autohandler_Task;
   begin
      Result.Initialize (State, Faction);
      return Player_Access (Result);
   end Create_Autohandler;

   --------------------
   -- Set_Autoplayer --
   --------------------

   procedure Set_Autoplayer
     (Handler    : Player_Access;
      Autoplayer : Autoplayer_Interface'Class)
   is
   begin
      Autohandler_Task_Access (Handler).Set_Autoplayer (Autoplayer);
   end Set_Autoplayer;

end Agrippa.Players.Autohandler;
