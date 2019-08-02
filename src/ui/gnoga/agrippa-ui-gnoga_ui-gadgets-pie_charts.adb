package body Agrippa.UI.Gnoga_UI.Gadgets.Pie_Charts is

   ------------
   -- Append --
   ------------

   procedure Append
     (Pie_Chart : in out Pie_Chart_Gadget_Type'Class;
      Color     : Agrippa.Colors.Agrippa_Color;
      Value     : Float)
   is
      Series : Charts.Chart_Series;
   begin
      Series.Set_Color (Color);
      Series.Append (Value);
      Pie_Chart.Append (Series);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Pie_Chart : in out Pie_Chart_Gadget_Type'Class;
      Color     : Agrippa.Colors.Agrippa_Color;
      Value     : Integer)
   is
   begin
      Pie_Chart.Append (Color, Float (Value));
   end Append;

   ------------
   -- Create --
   ------------

   procedure Create_Pie_Chart
     (Gadget  : in out Pie_Chart_Gadget_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Width, Height : Natural)
   is
   begin
      Gadget.Create_Chart (Parent, Width, Height);
   end Create_Pie_Chart;

   ------------------
   -- Render_Chart --
   ------------------

   overriding procedure Render_Chart
     (Gadget        : in out Pie_Chart_Gadget_Type;
      Width, Height : Natural;
      Context       : in out
        Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type)
   is

      Total   : Float := 0.0;
      Current : Float := 0.0;

      procedure Aggregate (Series : Charts.Chart_Series'Class);
      procedure Draw_Wedge (Series : Charts.Chart_Series'Class);

      ---------------
      -- Aggregate --
      ---------------

      procedure Aggregate (Series : Charts.Chart_Series'Class) is

         procedure Aggregate
           (X, Y    : Float;
            Is_Null : Boolean);

         ---------------
         -- Aggregate --
         ---------------

         procedure Aggregate
           (X, Y    : Float;
            Is_Null : Boolean)
         is
            pragma Unreferenced (X);
         begin
            if not Is_Null then
               Total := Total + Y;
            end if;
         end Aggregate;

      begin
         Series.Iterate (Aggregate'Access);
      end Aggregate;

      -----------------
      -- Draw_Series --
      -----------------

      procedure Draw_Wedge (Series : Charts.Chart_Series'Class) is

         procedure Draw
           (X, Y    : Float;
            Is_Null : Boolean);

         ----------
         -- Draw --
         ----------

         procedure Draw
           (X, Y    : Float;
            Is_Null : Boolean)
         is
            pragma Unreferenced (X);
            Size : constant Float :=
                     (if Is_Null then 0.0
                      else 360.0 * Y / Total);
         begin

            Context.Fill_Color
              (Agrippa.Colors.To_Html_Color_String
                 (Series.Color));

            Context.Begin_Path;
            Context.Move_To (Width / 2, Height / 2);
            Context.Arc_Degrees
              (X                 => Width / 2,
               Y                 => Height / 2,
               Radius            => Natural'Min (Width / 3, Height / 3),
               Starting_Angle    => Current,
               Ending_Angle      => Current + Size,
               Counter_Clockwise => False);
            Context.Fill;
            Current := Current + Size;

         end Draw;

      begin
         Series.Iterate (Draw'Access);
      end Draw_Wedge;

   begin
      Gadget.Iterate (Aggregate'Access);
      Gadget.Iterate (Draw_Wedge'Access);
   end Render_Chart;

end Agrippa.UI.Gnoga_UI.Gadgets.Pie_Charts;
