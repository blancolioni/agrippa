with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Agrippa.Json is

   type Null_Json_Value is
     new Json_Value with null record;

   overriding function Serialize
     (Value : Null_Json_Value)
      return String;

   type String_Json_Value is
     new Json_Value with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Serialize
     (Value : String_Json_Value)
      return String;

   type Integer_Json_Value is
     new Json_Value with
      record
         Value : Integer;
      end record;

   overriding function Serialize
     (Value : Integer_Json_Value)
      return String;

   type Boolean_Json_Value is
     new Json_Value with
      record
         Value : Boolean;
      end record;

   overriding function Serialize
     (Value : Boolean_Json_Value)
      return String;

   ------------
   -- Append --
   ------------

   procedure Append
     (To : in out Json_Array'Class; Value : Json_Value'Class) is
   begin
      To.Vector.Append (Value);
   end Append;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Bool : Boolean) return Json_Value'Class is
   begin
      return Result : constant Boolean_Json_Value :=
        Boolean_Json_Value'
          (Value => Bool);
   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Int : Integer) return Json_Value'Class is
   begin
      return Result : constant Integer_Json_Value := Integer_Json_Value'
        (Value => Int);
   end Integer_Value;

   ----------------
   -- Null_Value --
   ----------------

   function Null_Value return Json_Value'Class is
   begin
      return Result : Null_Json_Value;
   end Null_Value;

   ---------------
   -- Serialize --
   ---------------

   overriding function Serialize (Value : Json_Object) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Position in Value.Properties.Iterate loop
         if Result /= Null_Unbounded_String then
            Result := Result & ",";
         end if;
         Result := Result & """" & Json_Value_Maps.Key (Position)
           & """:" & Json_Value_Maps.Element (Position).Serialize;
      end loop;
      return "{" & To_String (Result) & "}";
   end Serialize;

   ---------------
   -- Serialize --
   ---------------

   overriding function Serialize (Value : Json_Array) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Item of Value.Vector loop
         if Result /= Null_Unbounded_String then
            Result := Result & ",";
         end if;
         Result := Result & Item.Serialize;
      end loop;
      return "[" & To_String (Result) & "]";
   end Serialize;

   overriding function Serialize
     (Value : Null_Json_Value)
      return String
   is ("null");

   overriding function Serialize
     (Value : String_Json_Value)
      return String
   is (""""
       & Ada.Strings.Unbounded.To_String (Value.Text)
       & """");

   overriding function Serialize
     (Value : Integer_Json_Value)
      return String
   is (Ada.Strings.Fixed.Trim (Value.Value'Image, Ada.Strings.Left));

   overriding function Serialize
     (Value : Boolean_Json_Value)
      return String
   is (if Value.Value then "true" else "false");

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Json_Value'Class)
   is
   begin
      if Object.Properties.Contains (Name) then
         Object.Properties.Replace (Name, Value);
      else
         Object.Properties.Insert (Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : String)
   is
   begin
      Object.Set_Property (Name, String_Value (Value));
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Integer)
   is
   begin
      Object.Set_Property (Name, Integer_Value (Value));
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Boolean)
   is
   begin
      Object.Set_Property (Name, Boolean_Value (Value));
   end Set_Property;

   ------------------
   -- String_Value --
   ------------------

   function String_Value (Text : String) return Json_Value'Class is
   begin
      return Result : constant String_Json_Value := String_Json_Value'
        (Text => Ada.Strings.Unbounded.To_Unbounded_String (Text));
   end String_Value;

end Agrippa.Json;
