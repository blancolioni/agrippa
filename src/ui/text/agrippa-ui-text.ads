with Agrippa.Game;

package Agrippa.UI.Text is

   type Text_UI_Type is
     limited new UI_Interface with private;

   overriding procedure Start
     (Text_UI  : in out Text_UI_Type);

private

   type Text_UI_Type is
     limited new UI_Interface with
      record
         Game : Agrippa.Game.Game_Access;
      end record;

end Agrippa.UI.Text;
