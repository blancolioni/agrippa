with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;

with Agrippa.UI.Gnoga_UI.Gadgets.Charts;

with Agrippa.Colors;

package Agrippa.UI.Gnoga_UI.Gadgets.Pie_Charts is

   type Pie_Chart_Gadget_Type is
     new Charts.Chart_Gadget_Type with private;

   overriding procedure Render_Chart
     (Gadget        : in out Pie_Chart_Gadget_Type;
      Width, Height : Natural;
      Context       : in out
        Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type);

   procedure Create_Pie_Chart
     (Gadget        : in out Pie_Chart_Gadget_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Width, Height : Natural);

   procedure Append
     (Pie_Chart : in out Pie_Chart_Gadget_Type'Class;
      Color     : Agrippa.Colors.Agrippa_Color;
      Value     : Float);

   procedure Append
     (Pie_Chart : in out Pie_Chart_Gadget_Type'Class;
      Color     : Agrippa.Colors.Agrippa_Color;
      Value     : Integer);

private

   type Pie_Chart_Gadget_Type is
     new Charts.Chart_Gadget_Type with null record;

end Agrippa.UI.Gnoga_UI.Gadgets.Pie_Charts;
