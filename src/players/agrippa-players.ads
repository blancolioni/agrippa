with Agrippa.Messages;
with Agrippa.State;

package Agrippa.Players is

   type Player_Interface is interface;

   function Name (Player : Player_Interface) return String
                  is abstract;

   procedure Initialize
     (Player  : in out Player_Interface;
      State   : in out Agrippa.State.State_Interface'Class;
      Faction : Faction_Id)
   is abstract;

   procedure Start_Turn
     (Player : in out Player_Interface;
      State  : in out Agrippa.State.State_Interface'Class)
   is abstract;

   function Send_Message
     (Player  : Player_Interface;
      State   : Agrippa.State.State_Interface'Class;
      Message : Agrippa.Messages.Message_Type)
      return Agrippa.Messages.Message_Type
      is abstract;

end Agrippa.Players;
