with Agrippa.Game;
with Agrippa.State.Notifications;

package Agrippa.UI.Text is

   type Text_UI_Type is
     limited new UI_Interface with private;

   overriding procedure Start
     (Text_UI  : in out Text_UI_Type);

   function Get_Text_Notifier
     return Agrippa.State.Notifications.Change_Handler_Interface'Class;

private

   type Text_UI_Type is
     limited new UI_Interface with
      record
         Game : Agrippa.Game.Game_Type;
      end record;

end Agrippa.UI.Text;
