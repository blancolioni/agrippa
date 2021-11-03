with Ada.Strings.Fixed;

package body Agrippa.Models.Tables is

   function "+" (X : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   ----------------
   -- Add_Column --
   ----------------

   procedure Add_Column
     (Table : in out Root_Table_Model'Class;
      Name  : String)
   is
   begin
      Table.Column_Names.Append (Name);
   end Add_Column;

   -------------
   -- Add_Row --
   -------------

   function Add_Row
     (Table     : in out Root_Table_Model'Class;
      Row_Model : access Root_Agrippa_Model'Class := null)
      return Table_Row_Index
   is
   begin
      Table.Rows.Append
        (Row_Record'
           (Cells     => <>,
            Row_Model => Model_Type (Row_Model)));
      for I in 1 .. Table.Column_Count loop
         Table.Rows (Table.Rows.Last_Index)
           .Cells.Append
             (Cell_Record'(Boolean_Cell, False));
      end loop;
      return Table.Rows.Last_Index;
   end Add_Row;

   -------------------
   -- Clear_Changes --
   -------------------

   procedure Clear_Changes
     (Model   : in out Root_Table_Model'Class)
   is
   begin
      Model.Changes := (Changes => <>);
   end Clear_Changes;

   ----------------
   -- Clear_Rows --
   ----------------

   procedure Clear_Rows
     (Table : in out Root_Table_Model'Class)
   is
   begin
      Table.Rows.Clear;
   end Clear_Rows;

   -----------------
   -- Column_Name --
   -----------------

   function Column_Name
     (Table  : Root_Table_Model'Class;
      Column : Table_Column_Index)
      return String
   is
   begin
      return Table.Column_Names.Element (Column);
   end Column_Name;

   -----------------
   -- Get_Changes --
   -----------------

   procedure Get_Changes
     (Model       : in out Root_Table_Model'Class;
      Changes     :    out Table_Change_List;
      Last_Change : in out Table_Change_Cursor)
   is
      use Table_Change_Lists;
      Position : Cursor := Cursor (Last_Change);
   begin
      if Has_Element (Position) then
         Changes.Changes.Clear;
         while Has_Element (Next (Position)) loop
            Next (Position);
            Changes.Changes.Append (Element (Position));
         end loop;
      else
         Changes := Model.Changes;
         Last_Change := Table_Change_Cursor (Model.Changes.Changes.Last);
      end if;
   end Get_Changes;

   -----------
   -- Image --
   -----------

   function Image (Cell : Cell_Record) return String is
   begin
      case Cell.T is
         when String_Cell =>
            return Ada.Strings.Unbounded.To_String (Cell.String_Value);
         when Integer_Cell =>
            return Ada.Strings.Fixed.Trim
              (Integer'Image (Cell.Integer_Value), Ada.Strings.Left);
         when Boolean_Cell =>
            return (if Cell.Boolean_Value then "true" else "false");
         when Abstract_Cell =>
            return (if Cell.Abstract_Value = null then "null"
                    else Cell.Abstract_Value.To_String);
      end case;
   end Image;

   ------------------
   -- Scan_Changes --
   ------------------

   procedure Scan_Changes
     (List    : Table_Change_List;
      Process : not null access
        procedure (Change : Table_Change))
   is
   begin
      for Item of List.Changes loop
         Process (Item);
      end loop;
   end Scan_Changes;

   --------------
   -- Set_Cell --
   --------------

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Boolean)
   is
   begin
      Table.Rows (Row).Cells.Replace_Element
        (Column, (Boolean_Cell, Value));
   end Set_Cell;

   --------------
   -- Set_Cell --
   --------------

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Integer)
   is
   begin
      Table.Rows (Row).Cells.Replace_Element
        (Column, (Integer_Cell, Value));
   end Set_Cell;

   --------------
   -- Set_Cell --
   --------------

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : String)
   is
   begin
      Table.Rows (Row).Cells.Replace_Element
        (Column, (String_Cell, +Value));
      Table.Changes.Changes.Append ((Cell_Contents_Changed, (Row, Column)));
   end Set_Cell;

   -----------
   -- Title --
   -----------

   overriding function Title
     (Model : Root_Table_Model)
      return String
   is
      pragma Unreferenced (Model);
   begin
      return "table";
   end Title;

end Agrippa.Models.Tables;
