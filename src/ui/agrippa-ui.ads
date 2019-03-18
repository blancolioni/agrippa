package Agrippa.UI is

   type UI_Interface is limited interface;

   procedure Start
     (UI       : in out UI_Interface)
   is abstract;

end Agrippa.UI;
