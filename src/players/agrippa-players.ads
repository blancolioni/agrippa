with Agrippa.Deals;
with Agrippa.Messages;
with Agrippa.State;

package Agrippa.Players is

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
     (Player  : Autoplayer_Interface;
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
     (Player : Autoplayer_Interface;
      State  : Agrippa.State.State_Interface'Class;
      Deal   : Agrippa.Deals.Deal_Type)
      return Boolean
      is abstract;

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

   type Player_Access is access all Player_Handler'Class;

end Agrippa.Players;
