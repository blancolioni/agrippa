package body Agrippa.UI.Gnoga_UI.Gadgets is

   -------------------
   -- Create_Gadget --
   -------------------

   procedure Create_Gadget
     (Gadget  : in out Agrippa_Gadget_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      Gadget.Create
        (Parent  => Parent,
         Content => "",
         ID      => "");
   end Create_Gadget;

end Agrippa.UI.Gnoga_UI.Gadgets;
