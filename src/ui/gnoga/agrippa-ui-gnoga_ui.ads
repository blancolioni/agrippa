package Agrippa.UI.Gnoga_UI is

   type Gnoga_UI_Type is
     limited new UI_Interface with private;

   overriding procedure Start
     (UI  : in out Gnoga_UI_Type);

   procedure Stop_Server (Message : String);

private

   type Gnoga_UI_Type is
   limited new UI_Interface with null record;

end Agrippa.UI.Gnoga_UI;
