package body Agrippa.UI.Gnoga_UI.Gadgets.Charts is

   ------------
   -- Append --
   ------------

   procedure Append
     (Series : in out Chart_Series'Class;
      Value  : Float)
   is
   begin
      Series.Data.Append
        (Data_Point'
           (X       => Float (Series.Data.Length) + 1.0,
            Y       => Value,
            Is_Null => False));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Series : in out Chart_Series'Class;
      Value  : Integer)
   is
   begin
      Series.Append (Float (Value));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Gadget : in out Chart_Gadget_Type;
      Series : Chart_Series'Class)
   is
   begin
      Gadget.Series.Append (Chart_Series (Series));
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Gadget : in out Chart_Gadget_Type)
   is
   begin
      Gadget.Series.Clear;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Series : in out Chart_Series'Class)
   is
   begin
      Series.Data.Clear;
   end Clear;

   ------------
   -- Create --
   ------------

   procedure Create_Chart
     (Gadget  : in out Chart_Gadget_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Width, Height : Natural)
   is
   begin
      Gadget.Create_Gadget (Parent);
      Gadget.Canvas.Create (Gadget, Width, Height);
      Gadget.Context.Get_Drawing_Context_2D (Gadget.Canvas);
      Gadget.Render;
   end Create_Chart;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Series  : Chart_Series'Class;
      Process : not null access
        procedure (X, Y : Float;
                   Is_Null : Boolean))
   is
   begin
      for Data of Series.Data loop
         Process (Data.X, Data.Y, Data.Is_Null);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Gadget  : Chart_Gadget_Type;
      Process : not null access
        procedure (Series : Chart_Series'Class))
   is
   begin
      for Series of Gadget.Series loop
         Process (Series);
      end loop;
   end Iterate;

   ------------
   -- Render --
   ------------

   procedure Render
     (Gadget  : in out Chart_Gadget_Type)
   is
   begin
      Chart_Gadget_Type'Class (Gadget).Render_Chart
        (Gadget.Canvas.Width, Gadget.Canvas.Height, Gadget.Context);
   end Render;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Series : in out Chart_Series'Class;
      Color  : Agrippa.Colors.Agrippa_Color)
   is
   begin
      Series.Color := Color;
   end Set_Color;

end Agrippa.UI.Gnoga_UI.Gadgets.Charts;
