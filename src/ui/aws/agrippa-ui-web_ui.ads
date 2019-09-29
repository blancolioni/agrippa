package Agrippa.UI.Web_UI is

   type Web_UI_Type is
     limited new UI_Interface with private;

   overriding procedure Start
     (Web_UI  : in out Web_UI_Type);

private

   type Web_UI_Type is
     limited new UI_Interface with
      record
         null;
      end record;

end Agrippa.UI.Web_UI;
