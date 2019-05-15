with Gnoga.Gui.Base;
with Gnoga.Gui.Element;

private with Gnoga.Gui.Element.Common;

package Agrippa.UI.Gnoga_UI.Gadgets is

   type Agrippa_Gadget_Type is
     abstract new Gnoga.Gui.Element.Element_Type with private;

   procedure Create_Gadget
     (Gadget        : in out Agrippa_Gadget_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class);

private

   type Agrippa_Gadget_Type is
     abstract new Gnoga.Gui.Element.Common.DIV_Type with
      record
         null;
      end record;

end Agrippa.UI.Gnoga_UI.Gadgets;
