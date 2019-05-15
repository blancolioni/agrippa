with Gnoga.Gui.Base;
with Gnoga.Gui.Element;

private with Gnoga.Gui.Element.Common;

with Agrippa.Models;

package Agrippa.UI.Gnoga_UI.Views is

   type Agrippa_View_Type is
     new Gnoga.Gui.Element.Element_Type with private;

   procedure Create
     (View    : in out Agrippa_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String := "";
      Model   : Agrippa.Models.Model_Type := null);

   procedure Set_Model
     (View  : in out Agrippa_View_Type'Class;
      Model : Agrippa.Models.Model_Type);

private

   type Agrippa_View_Type is
     new Gnoga.Gui.Element.Common.DIV_Type with
      record
         Model : Agrippa.Models.Model_Type;
      end record;

end Agrippa.UI.Gnoga_UI.Views;
