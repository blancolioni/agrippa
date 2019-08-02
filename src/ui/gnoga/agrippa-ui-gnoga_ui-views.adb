with Gnoga.Gui.Element.Table;

with Agrippa.Models.Tables;

package body Agrippa.UI.Gnoga_UI.Views is

   procedure Load_Table
     (View  : in out Agrippa_View_Type'Class;
      Model : Agrippa.Models.Tables.Table_Model);

   ------------
   -- Create --
   ------------

   procedure Create
     (View    : in out Agrippa_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String := "";
      Model   : Agrippa.Models.Model_Type := null)
   is
   begin
      Gnoga.Gui.Element.Common.DIV_Type (View).Create
        (Parent => Parent,
         ID     => ID);
      Agrippa_View_Type'Class (View).Set_Model (Model);
   end Create;

   ----------------
   -- Load_Table --
   ----------------

   procedure Load_Table
     (View  : in out Agrippa_View_Type'Class;
      Model : Agrippa.Models.Tables.Table_Model)
   is
      use Gnoga.Gui.Element.Table;
      Header      : Table_Header_Type;
      Table       : Table_Type;
      Tbody       : Table_Body_Type;
   begin
      Table.Create (View);
      Table.Class_Name ("agrippaTable");

      declare
         Heading_Row : Table_Row_Type;
      begin
         Header.Create (Table);
         Heading_Row.Create (Header);

         for Column_Index in 1 .. Model.Column_Count loop
            declare
               Heading : Table_Heading_Type;
            begin
               Heading.Create
                 (Row         => Heading_Row,
                  Content     => Model.Column_Name (Column_Index));
            end;
         end loop;
      end;

      Tbody.Create (Table);

      for Row_Index in 1 .. Model.Row_Count loop
         declare
            Row   : Table_Row_Type;
         begin
            Row.Create (Tbody);
            for Column_Index in 1 .. Model.Column_Count loop
               declare
                  Cell : Pointer_To_Table_Column_Class;
               begin
                  Cell := new Table_Column_Type;
                  Cell.Create
                    (Row, Model.Image (Row_Index, Column_Index));
               end;
            end loop;
         end;
      end loop;

   end Load_Table;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (View  : in out Agrippa_View_Type'Class;
      Model : Agrippa.Models.Model_Type)
   is
      use type Agrippa.Models.Model_Type;
   begin
      View.Model := Model;
      if Model = null then
         View.Inner_HTML ("");
      elsif Model.all in Agrippa.Models.Tables.Root_Table_Model'Class then
         View.Load_Table
           (Agrippa.Models.Tables.Table_Model (Model));
      else
         View.Inner_HTML (Model.Title);
      end if;
   end Set_Model;

end Agrippa.UI.Gnoga_UI.Views;
