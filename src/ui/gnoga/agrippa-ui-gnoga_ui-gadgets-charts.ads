private with Ada.Containers.Doubly_Linked_Lists;

with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;

with Agrippa.Colors;

package Agrippa.UI.Gnoga_UI.Gadgets.Charts is

   type Chart_Series is tagged private;

   function Color
     (Series : Chart_Series'Class)
      return Agrippa.Colors.Agrippa_Color;

   procedure Set_Color
     (Series : in out Chart_Series'Class;
      Color  : Agrippa.Colors.Agrippa_Color);

   procedure Append
     (Series : in out Chart_Series'Class;
      Value  : Float);

   procedure Append
     (Series : in out Chart_Series'Class;
      Value  : Integer);

   procedure Clear
     (Series : in out Chart_Series'Class);

   procedure Iterate
     (Series  : Chart_Series'Class;
      Process : not null access
        procedure (X, Y : Float;
                   Is_Null : Boolean));

   type Chart_Gadget_Type is
     abstract new Agrippa_Gadget_Type with private;

   procedure Create_Chart
     (Gadget        : in out Chart_Gadget_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Width, Height : Natural);

   procedure Render
     (Gadget  : in out Chart_Gadget_Type);

   procedure Render_Chart
     (Gadget        : in out Chart_Gadget_Type;
      Width, Height : Natural;
      Context       : in out
        Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type)
   is abstract;

   procedure Append
     (Gadget : in out Chart_Gadget_Type;
      Series : Chart_Series'Class);

   procedure Clear
     (Gadget : in out Chart_Gadget_Type);

   procedure Iterate
     (Gadget : Chart_Gadget_Type;
      Process : not null access
        procedure (Series : Chart_Series'Class));

private

   type Data_Point is
      record
         X, Y    : Float;
         Is_Null : Boolean;
      end record;

   package Data_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Data_Point);

   type Chart_Series is tagged
      record
         Color : Agrippa.Colors.Agrippa_Color;
         Data  : Data_Lists.List;
      end record;

   function Color
     (Series : Chart_Series'Class)
      return Agrippa.Colors.Agrippa_Color
   is (Series.Color);

   package Series_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Chart_Series);

   type Chart_Gadget_Type is
     abstract new Agrippa_Gadget_Type with
      record
         Series  : Series_Lists.List;
         Canvas  : Gnoga.Gui.Element.Canvas.Canvas_Type;
         Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      end record;

end Agrippa.UI.Gnoga_UI.Gadgets.Charts;
