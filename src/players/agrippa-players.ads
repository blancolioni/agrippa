with Agrippa.Deals;
with Agrippa.Messages;
with Agrippa.Proposals;
with Agrippa.State;

package Agrippa.Players is

   type Player_Handler is synchronized interface;

   procedure Initialize
     (Player  : in out Player_Handler;
      State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
   is abstract;

   procedure Stop
     (Player : in out Player_Handler)
   is abstract;

   procedure Start_Turn
     (Player  : in out Player_Handler;
      State   : in out Agrippa.State.State_Interface'Class)
   is abstract;

   procedure Send_Message
     (Player  : in out Player_Handler;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
   is abstract;

   procedure Get_Reply
     (Player : in out Player_Handler;
      Reply  :    out Agrippa.Messages.Message_Type)
   is abstract;

   procedure Senate_Phase_Desire
     (Player : Player_Handler;
      State  : Agrippa.State.State_Interface'Class)
      is abstract;

   procedure What_Do_You_Want_For
     (Player : in out Player_Handler;
      State   : Agrippa.State.State_Interface'Class;
      Offer   : Agrippa.Deals.Offer_List)
   is abstract;

   procedure What_Will_You_Give_For
     (Player : in out Player_Handler;
      State   : Agrippa.State.State_Interface'Class;
      Offer   : Agrippa.Deals.Offer_List)
   is abstract;

   procedure Get_Offer_Reply
     (Player : in out Player_Handler;
      Offer  :    out Agrippa.Deals.Offer_List)
   is abstract;

   procedure Will_You_Agree_To
     (Player : in out Player_Handler;
      State   : Agrippa.State.State_Interface'Class;
      Deal    : Agrippa.Deals.Deal_Type)
   is abstract;

   procedure Get_Agreement_Reply
     (Player : in out Player_Handler;
      Agree  :    out Boolean)
   is abstract;

   procedure Vote_Proposal
     (Player   : in out Player_Handler;
      State    : Agrippa.State.State_Interface'Class;
      Proposal : Agrippa.Proposals.Proposal_Container_Type)
   is abstract;

   procedure Get_Votes
     (Player   : in out Player_Handler;
      Votes    :    out Faction_Vote_Type)
   is abstract;

   type Player_Access is access all Player_Handler'Class;

   type Player_Access_Array is
     array (Faction_Id) of Player_Access;

   type Autoplayer_Interface is interface;

   function Name (Player : Autoplayer_Interface) return String
                  is abstract;

   procedure Initialize
     (Player  : in out Autoplayer_Interface;
      State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
   is abstract;

   procedure Start_Turn
     (Player : Autoplayer_Interface;
      State  : in out Agrippa.State.State_Interface'Class)
   is abstract;

   function Send_Message
     (Player  : in out Autoplayer_Interface;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
      return Agrippa.Messages.Message_Type
      is abstract;

   function Senate_Phase_Desire
     (Player : Autoplayer_Interface;
      State  : Agrippa.State.State_Interface'Class)
      return Agrippa.Deals.Offer_List
      is abstract;

   function What_Do_You_Want_For
     (Player : Autoplayer_Interface;
      State  : Agrippa.State.State_Interface'Class;
      Offer  : Agrippa.Deals.Offer_List)
      return Agrippa.Deals.Offer_List
      is abstract;

   function What_Will_You_Give_For
     (Player : Autoplayer_Interface;
      State  : Agrippa.State.State_Interface'Class;
      Offer  : Agrippa.Deals.Offer_List)
      return Agrippa.Deals.Offer_List
      is abstract;

   function Will_You_Agree_To
     (Player : in out Autoplayer_Interface;
      State  : Agrippa.State.State_Interface'Class;
      Deal   : Agrippa.Deals.Deal_Type)
      return Boolean
      is abstract;

   function Vote
     (Player    : Autoplayer_Interface;
      State     : Agrippa.State.State_Interface'Class;
      Proposals : Agrippa.Proposals.Proposal_Container_Type)
      return Faction_Vote_Type
      is abstract;

   function Get_Player_Handler
     (Player : Autoplayer_Interface)
      return Player_Access
      is abstract;

   procedure Set_Players
     (Player  : in out Autoplayer_Interface;
      Players : Player_Access_Array)
   is abstract;

end Agrippa.Players;
