private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Agrippa.Models.Tables is

   type Abstract_Cell_Interface is interface;

   function To_String
     (Cell : Abstract_Cell_Interface)
      return String
      is abstract;

   type Table_Row_Count is new Natural;
   subtype Table_Row_Index is
     Table_Row_Count range 1 .. Table_Row_Count'Last;

   type Table_Column_Count is new Natural;
   subtype Table_Column_Index is
     Table_Column_Count range 1 .. Table_Column_Count'Last;

   type Root_Table_Model is
     new Root_Agrippa_Model with private;

   type Table_Model is access all Root_Table_Model'Class;

   overriding function Title
     (Model : Root_Table_Model)
      return String;

   procedure Clear_Rows
     (Table : in out Root_Table_Model'Class);

   function Row_Count
     (Table : Root_Table_Model'Class)
      return Table_Row_Count;

   function Row_Model
     (Table : Root_Table_Model'Class;
      Row   : Table_Row_Index)
      return Model_Type;

   function Column_Count
     (Table : Root_Table_Model'Class)
      return Table_Column_Count;

   function Column_Name
     (Table  : Root_Table_Model'Class;
      Column : Table_Column_Index)
      return String;

   function Image
     (Table  : Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index)
      return String;

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Boolean);

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : Integer);

   procedure Set_Cell
     (Table  : in out Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index;
      Value  : String);

   procedure Add_Column
     (Table : in out Root_Table_Model'Class;
      Name  : String);

   function Add_Row
     (Table     : in out Root_Table_Model'Class;
      Row_Model : access Root_Agrippa_Model'Class := null)
      return Table_Row_Index;

   type Cell_Position is
      record
         Row : Table_Row_Index;
         Col : Table_Column_Index;
      end record;

   type Table_Change_Type is
     (Cell_Contents_Changed,
      Row_Added,
      Row_Deleted);

   type Table_Change (Change_Type : Table_Change_Type) is private;

   function Get_Cell_Position (Change : Table_Change) return Cell_Position
     with Pre => Change.Change_Type = Cell_Contents_Changed;

   type Table_Change_List is private;

   type Table_Change_Cursor is private;

   function Current_Change
     (Model : Root_Table_Model'Class)
      return Table_Change_Cursor;

   procedure Get_Changes
     (Model       : in out Root_Table_Model'Class;
      Changes     :    out Table_Change_List;
      Last_Change : in out Table_Change_Cursor);

   procedure Clear_Changes
     (Model   : in out Root_Table_Model'Class);

   procedure Scan_Changes
     (List        : Table_Change_List;
      Process     : not null access
        procedure (Change : Table_Change));

private

   type Table_Change (Change_Type : Table_Change_Type) is
      record
         case Change_Type is
            when Cell_Contents_Changed =>
               Position : Cell_Position;
            when Row_Added | Row_Deleted =>
               Row      : Table_Row_Index;
         end case;
      end record;

   function Get_Cell_Position (Change : Table_Change) return Cell_Position
   is (Change.Position);

   package Table_Change_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Table_Change);

   type Table_Change_Cursor is
     new Table_Change_Lists.Cursor;

   type Table_Change_List is
      record
         Changes : Table_Change_Lists.List;
      end record;

   type Cell_Type is
     (String_Cell,
      Integer_Cell,
      Boolean_Cell,
      Abstract_Cell);

   type Cell_Record (T : Cell_Type) is
      record
         case T is
            when String_Cell =>
               String_Value   : Ada.Strings.Unbounded.Unbounded_String;
            when Integer_Cell =>
               Integer_Value  : Integer;
            when Boolean_Cell =>
               Boolean_Value  : Boolean;
            when Abstract_Cell =>
               Abstract_Value : access Abstract_Cell_Interface'Class;
         end case;
      end record;

   function Image (Cell : Cell_Record) return String;

   package Cell_Vectors is
     new Ada.Containers.Indefinite_Vectors (Table_Column_Index, Cell_Record);

   type Row_Record is
      record
         Cells     : Cell_Vectors.Vector;
         Row_Model : Model_Type;
      end record;

   package Column_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Table_Column_Index, String);

   package Row_Vectors is
     new Ada.Containers.Vectors (Table_Row_Index, Row_Record);

   type Root_Table_Model is
     new Root_Agrippa_Model with
      record
         Column_Names : Column_Name_Vectors.Vector;
         Rows         : Row_Vectors.Vector;
         Changes      : Table_Change_List;
      end record;

   function Row_Count
     (Table : Root_Table_Model'Class)
      return Table_Row_Count
   is (Table.Rows.Last_Index);

   function Column_Count
     (Table : Root_Table_Model'Class)
      return Table_Column_Count
   is (Table.Column_Names.Last_Index);

   function Image
     (Table  : Root_Table_Model'Class;
      Row    : Table_Row_Index;
      Column : Table_Column_Index)
      return String
   is (Image (Table.Rows (Row).Cells (Column)));

   function Current_Change
     (Model : Root_Table_Model'Class)
      return Table_Change_Cursor
   is (Table_Change_Cursor (Model.Changes.Changes.Last));

   function Row_Model
     (Table : Root_Table_Model'Class;
      Row   : Table_Row_Index)
      return Model_Type
   is (Table.Rows (Row).Row_Model);

end Agrippa.Models.Tables;
