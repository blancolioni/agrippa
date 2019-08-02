with Gnoga.Gui.Element.Table;

with Agrippa.State.Treasury;

package body Agrippa.UI.Gnoga_UI.Phase_Views.Revenue is

   type Revenue_Phase_Type is
     new Agrippa_Phase_View_Type with
      record
         Table : Gnoga.Gui.Element.Table.Table_Type;
      end record;

   overriding function Phase_Name
     (Phase : Revenue_Phase_Type)
      return String
   is ("revenue-phase");

   overriding procedure Create_Phase_View
     (View    : in out Revenue_Phase_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class);

   overriding procedure Activate
     (View    : in out Revenue_Phase_Type);

   procedure Create_Revenue_Table
     (View : in out Revenue_Phase_Type'Class);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (View    : in out Revenue_Phase_Type)
   is
   begin
      View.Table.Inner_HTML ("");
      View.Create_Revenue_Table;
   end Activate;

   -----------------------
   -- Create_Phase_View --
   -----------------------

   overriding procedure Create_Phase_View
     (View    : in out Revenue_Phase_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      View.Create (Parent);
      View.Table.Create (View);
      View.Table.Class_Name ("revenue-table");
      View.Create_Revenue_Table;
   end Create_Phase_View;

   --------------------------
   -- Create_Revenue_Table --
   --------------------------

   procedure Create_Revenue_Table
     (View : in out Revenue_Phase_Type'Class)
   is
      use Gnoga.Gui.Element.Table;
      Table_Body  : Table_Body_Type;
      Items       : constant Agrippa.State.Treasury.Array_Of_Revenue_Items :=
                      Agrippa.State.Treasury.Calculate_Revenue
                        (View.State);
   begin
      Table_Body.Create (View.Table);

      for Item of Items loop
         declare
            use all type Agrippa.State.Treasury.Revenue_Item_Type;
            Row : Table_Row_Type;
            Blank_Cell : Table_Column_Type;
            Name_Cell : Table_Column_Type;
            Value_Cell : Table_Column_Type;
         begin
            Row.Create (Table_Body);
            case Agrippa.State.Treasury.Item_Type (Item) is
               when Section_Header | Main_Item =>
                  Name_Cell.Create
                    (Row         => Row,
                     Content     =>
                        "<B>" & Agrippa.State.Treasury.Name (Item) & "</B>",
                     Column_Span => 2);
               when Normal_Item =>
                  Blank_Cell.Create (Row);
                  Blank_Cell.Class_Name ("blank-cell");
                  Name_Cell.Create (Row, Agrippa.State.Treasury.Name (Item));
            end case;

            Value_Cell.Create
              (Row         => Row,
               Content     =>
                 Integer'Image (Agrippa.State.Treasury.Amount (Item)));
         end;
      end loop;

   end Create_Revenue_Table;

   -------------------
   -- Revenue_Phase --
   -------------------

   function Revenue_Phase
     (State : Agrippa.State.Agrippa_State)
      return Phase_View
   is
   begin
      return Phase : constant Phase_View := new Revenue_Phase_Type do
         Phase.State := State;
      end return;
   end Revenue_Phase;

end Agrippa.UI.Gnoga_UI.Phase_Views.Revenue;
